module DFA where

import RegEx
import Trace

import Control.Arrow
import Control.Lens as Lens
import Control.Monad.State.Strict
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
    current = Trace.state trace
    accepted' = current `Set.member` (dfa ^. final)

start :: (Ord s) => DFA s a -> Trace s a
start dfa = over path ((dfa ^. initial) <|) >>> adjust dfa $ nil

add :: (Ord s) => DFA s a -> Trace s a -> a -> s -> Trace s a
add dfa trace input current =
  over path (current <|) >>> over transitions (input <|) >>> adjust dfa $ trace

characters :: (Ord s, Ord a) => DFA s a -> s -> Set a
characters dfa state =
  case Map.lookup state (dfa ^. table) of
    Nothing -> Set.empty
    Just behavior -> Set.fromList $ Map.keys behavior

states :: (Ord s) => DFA s a -> s -> Set s
states dfa state =
  case Map.lookup state (dfa ^. table) of
    Nothing -> Set.empty
    Just behavior -> Set.fromList $ Map.elems behavior

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
      let current = Trace.state trace
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

intoRegEx ::
     forall s a. (Ord s)
  => DFA s a
  -> RegEx a
intoRegEx dfa = evalState intoRegEx' Map.empty
  where
    table' = dfa ^. table
    initial' = dfa ^. initial
    final' = dfa ^. final
    err' = dfa ^. err

    -- This is the set of all states. It is important to have this ready for
    -- when we compute the table.
    states =
      final' `Set.union`
      Set.fromList
        (concat
           [ [initial']
           , [err']
           , Map.keys table'
           , concat $ Map.elems <$> Map.elems table'
           ])

    intoRegEx' :: State (Map (Int, s, s) (RegEx a)) (RegEx a)
    intoRegEx' = do
      let n = Set.size states
      -- Fetch regexes that match from the initial state to all final states
      regexes <- mapM (fill n initial') $ Set.toList final'
      -- Concatenate them into a single RegEx
      return $ RegEx.any regexes

    fill ::
         (Ord s) => Int -> s -> s -> State (Map (Int, s, s) (RegEx a)) (RegEx a)
    fill 0 from to =
      if from == to then
        undefined
      else
        undefined
    fill level from to = undefined
