{-# LANGUAGE ScopedTypeVariables #-}

module NDFAL where

import Trace

import Control.Arrow
import Control.Lens as Lens
import Control.Monad.Loops
import Control.Monad.State.Strict
import Data.Foldable as Foldable
import Data.Map.Strict as Map hiding (adjust)
import Data.Maybe (fromMaybe)
import Data.Set as Set hiding (adjust)

data Lambda =
  Lambda
  deriving (Show, Eq, Ord)

type Lam a = Either Lambda a

lambda :: Lam a
lambda = Left Lambda

type NDFALTable s a = Map s (Map (Lam a) (Set s))

data NDFAL s a = NDFAL
  { _initial :: !s
  -- ^ Initial state.
  , _final :: !(Set s)
  -- ^ Set of states in which we accept the input.
  , _table :: !(NDFALTable s a)
  -- ^ State transition function.
  }

makeLenses ''NDFAL

adjust :: (Ord s) => NDFAL s a -> Trace (Set s) (Lam a) -> Trace (Set s) (Lam a)
adjust ndfal trace = set accepted accepted' trace
  where
    current = Trace.state trace
    -- The automaton is in accept state if at least one of the current states
    -- is in the set of final states.
    accepted' = not $ current `Set.disjoint` (ndfal ^. final)

start :: (Ord s, Ord a) => NDFAL s a -> Trace (Set s) (Lam a)
start ndfal =
  over path (closure' ndfal [ndfal ^. initial] <|) >>> adjust ndfal $ nil

add ::
     (Ord s)
  => NDFAL s a
  -> Trace (Set s) (Lam a)
  -> Lam a
  -> Set s
  -> Trace (Set s) (Lam a)
add ndfal trace input current =
  over path (current <|) >>> over transitions (input <|) >>> adjust ndfal $
  trace

closure' :: (Ord s, Ord a, Foldable f) => NDFAL s a -> f s -> Set s
closure' ndfal = Prelude.foldr (\s a -> closure ndfal s `Set.union` a) Set.empty

closure ::
     forall s a f. (Ord s, Ord a)
  => NDFAL s a
  -> s
  -> Set s
closure ndfal = closure'
  where
    closure' :: s -> Set s
    closure' source =
      flip evalState Set.empty $
      snd <$>
      iterateUntilM
        (\(pending, _) -> Set.null pending)
        (\(pending, partial) -> do
           let (node, rest) = Set.deleteFindMin pending
           found <- _reachable node
           return (found `Set.union` rest, found `Set.union` partial))
        (Set.singleton source, Set.singleton source)
    behavior = ndfal ^. table
    _reachable :: s -> State (Set s) (Set s)
    _reachable source
      -- Add the current node to the already seen
     = do
      modify (Set.insert source)
      -- Check all its lambda transitions
      case Map.lookup source behavior
        -- Could not find the state. Just say we can't go anywhere.
            of
        Nothing -> return Set.empty
        Just behavior'
          -- Found the state, now fetch its lambda transitions
         ->
          let reachable =
                Map.findWithDefault (Set.singleton source) lambda behavior'
          -- Remove nodes that have already been seen
           in gets $ Set.difference reachable

delta ::
     (Ord s, Ord a, Functor f, Foldable f)
  => NDFAL s a
  -> f a
  -> Trace (Set s) (Lam a)
delta ndfal inputs = Prelude.foldl progress (start ndfal) inputs'
  where
    inputs' = Right <$> inputs
    behavior = ndfal ^. table
    state `transition` input =
      case Map.lookup state behavior of
        Nothing -> Set.empty
        Just behavior' -> Map.findWithDefault Set.empty input behavior'
    progress trace input = do
      let current = Trace.state trace
      let output = Set.unions $ (`transition` input) <$> Set.toList current
      add ndfal trace input $ closure' ndfal output

fromTable ::
     (Ord s, Ord a, Foldable f)
  => Map s (Map (Lam a) (Set s))
  -> s
  -> f s
  -> NDFAL s a
fromTable table start accept =
  NDFAL
    { _initial = start
    , _final = Set.fromList $ Foldable.toList accept
    , _table = table
    }
