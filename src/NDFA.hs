module NDFA where

import Trace

import Control.Arrow
import Control.Lens as Lens
import Data.Foldable as Foldable
import Data.Map.Strict as Map hiding (adjust)
import Data.Maybe (fromMaybe)
import Data.Set as Set hiding (adjust)

type NDFATable s a = Map s (Map a (Set s))

data NDFA s a = NDFA
  { _initial :: !s
  -- ^ Initial state.
  , _final :: !(Set s)
  -- ^ Set of states in which we accept the input.
  , _table :: !(NDFATable s a)
  -- ^ State transition function.
  }

makeLenses ''NDFA

adjust :: (Ord s) => NDFA s a -> Trace (Set s) a -> Trace (Set s) a
adjust ndfa trace = set accepted accepted' trace
  where
    current = state trace
    -- The automaton is in accept state if at least one of the current states
    -- is in the set of final states.
    accepted' = not $ Set.null $ current `Set.intersection` (ndfa ^. final)

start :: (Ord s) => NDFA s a -> Trace (Set s) a
start ndfa =
  over path (Set.singleton (ndfa ^. initial) <|) >>> adjust ndfa $ nil

add :: (Ord s) => NDFA s a -> Trace (Set s) a -> a -> Set s -> Trace (Set s) a
add ndfa trace input current =
  over path (current <|) >>> over transitions (input <|) >>> adjust ndfa $ trace

delta :: (Ord s, Ord a, Foldable f) => NDFA s a -> f a -> Trace (Set s) a
delta ndfa = Prelude.foldl progress (start ndfa)
  where
    behavior = ndfa ^. table
    state `transition` input =
      case Map.lookup state behavior of
        Nothing -> Set.empty
        Just behavior' -> Map.findWithDefault Set.empty input behavior'
    progress trace input = do
      let current = state trace
      let output = Set.unions $ (`transition` input) <$> Set.toList current
      add ndfa trace input output

fromTable :: (Ord s, Ord a, Foldable f) => NDFATable s a -> s -> f s -> NDFA s a
fromTable table start accept =
  NDFA
    { _initial = start
    , _final = Set.fromList $ Foldable.toList accept
    , _table = table
    }
