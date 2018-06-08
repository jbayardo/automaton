module DFA where

import Trace

import Control.Arrow
import Control.Lens as Lens
import Data.Foldable as Foldable
import Data.Map.Strict as Map hiding (adjust)
import Data.Maybe (fromMaybe)
import Data.Set as Set hiding (adjust)

type DFATable s a = Map s (Map a s)

data DFA s a = DFA
  { _initial :: !s
  -- ^ Initial state.
  , _final :: !(Set s)
  -- ^ Set of states in which we accept the input.
  , _table :: !(DFATable s a)
  -- ^ State transition function.
  , _err :: !s
  -- ^ State to which we transition if we had an execution failure.
  }

makeLenses ''DFA

adjust :: (Ord s) => DFA s a -> Trace s a -> Trace s a
adjust dfa trace = set accepted accepted' trace
  where
    current = state trace
    accepted' = current `Set.member` (dfa ^. final)

start :: (Ord s) => DFA s a -> Trace s a
start dfa = over path ((dfa ^. initial) <|) >>> adjust dfa $ nil

add :: (Ord s) => DFA s a -> Trace s a -> a -> s -> Trace s a
add dfa trace input current =
  over path (current <|) >>> over transitions (input <|) >>> adjust dfa $ trace

delta :: (Ord s, Ord a, Foldable f) => DFA s a -> f a -> Trace s a
delta dfa = Prelude.foldl progress (start dfa)
  where
    behavior = dfa ^. table
    failure = dfa ^. err
    state `transition` input =
      case Map.lookup state behavior of
        Nothing -> failure
        Just behavior' -> Map.findWithDefault failure input behavior'
    progress trace input = do
      let current = state trace
      let output = current `transition` input
      add dfa trace input output

fromTable ::
     (Ord s, Ord a, Foldable f) => DFATable s a -> s -> s -> f s -> DFA s a
-- TODO: ensure no transitions out of failure state
fromTable table start failure accept =
  DFA
    { _initial = start
    , _final = Set.fromList $ Foldable.toList accept
    , _table = table
    , _err = failure
    }
