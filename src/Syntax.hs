{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Syntax where

import Data.Map.Strict as Map
import Data.Set as Set
import NDFAL (Lam, Lambda(..))

class Lambdable a b where
  trans :: a -> Lam b

instance Lambdable a a where
  trans = Right

instance Lambdable (Lam a) a where
  trans = id

automaton :: (Ord k, Foldable f, Monoid v) => f (k, v) -> Map k v
automaton = Prelude.foldr (uncurry (Map.insertWith mappend)) Map.empty

with :: (Ord k, Foldable f, Monoid v) => s -> f (k, v) -> (s, Map k v)
state `with` transitions = (state, automaton transitions)

into :: a -> s -> (a, s)
into = (,)

into' :: (Foldable f, Ord s) => a -> f s -> (a, Set s)
into' b s = (b, Prelude.foldr Set.insert Set.empty s)

into'' :: (Lambdable b a, Foldable f, Ord s) => b -> f s -> (Lam a, Set s)
into'' b s = (trans b, Prelude.foldr Set.insert Set.empty s)
