{-# LANGUAGE ScopedTypeVariables #-}

module RegEx where

import Control.Monad.State.Strict as State
import Data.Map.Strict as Map
import Data.Set as Set
import NDFAL
import Syntax

data RegEx a
  = Concat (RegEx a)
           (RegEx a)
  | Sum (RegEx a)
        (RegEx a)
  | Kleene (RegEx a)
  | Match a
  | Epsilon
  | Empty
  deriving (Show)

compile :: (Ord a) => RegEx a -> NDFAL Int a
compile regex =
  let (table, start, accept) = evalState (intoTable regex) 0
   in NDFAL.fromTable table start accept

fresh :: State Int Int
fresh = state $ \s -> (s, s + 1)

intoTable ::
     forall a. (Ord a)
  => RegEx a
  -> State Int (NDFALTable Int a, Int, [Int])
intoTable Empty = do
  initial <- fresh
  final <- fresh
  return (Map.empty, initial, [final])
intoTable Epsilon = do
  initial <- fresh
  final <- fresh
  let table = automaton [initial `with` [(lambda :: Lam a) `into''` [final]]]
  return (table, initial, [final])
intoTable (Match x) = do
  initial <- fresh
  final <- fresh
  let table = automaton [initial `with` [x `into''` [final]]]
  return (table, initial, [final])
intoTable (Sum s r) = do
  initial <- fresh
  final <- fresh
  (table_s, initial_s, final_s) <- intoTable s
  (table_r, initial_r, final_r) <- intoTable r
  let table =
        automaton $
        concat
          [ [initial `with` [(lambda :: Lam a) `into''` [initial_s, initial_r]]]
          , [x `with` [(lambda :: Lam a) `into''` [final]] | x <- final_s]
          , [x `with` [(lambda :: Lam a) `into''` [final]] | x <- final_r]
          ]
  return (table `mappend` table_s `mappend` table_r, initial, [final])
intoTable (Concat s r) = do
  (table_s, initial_s, final_s) <- intoTable s
  (table_r, initial_r, final_r) <- intoTable r
  let table =
        automaton
          [x `with` [(lambda :: Lam a) `into''` [initial_r]] | x <- final_s]
  return (table `mappend` table_s `mappend` table_r, initial_s, final_r)
intoTable (Kleene r) = do
  initial <- fresh
  final <- fresh
  (table_r, initial_r, final_r) <- intoTable r
  let table =
        automaton $
        initial `with` [(lambda :: Lam a) `into''` [initial_r, final]] :
        [ fr `with` [(lambda :: Lam a) `into''` [initial_r, final]]
        | fr <- final_r
        ]
  return (table `mappend` table_r, initial, [final])

char :: a -> RegEx a
char = Match

many1 :: RegEx a -> RegEx a
many1 a = Concat a $ many a

many :: RegEx a -> RegEx a
many = Kleene

eps :: RegEx a
eps = Epsilon

perhaps :: RegEx a -> RegEx a
perhaps = Sum Epsilon

(<>) :: RegEx a -> RegEx a -> RegEx a
(<>) = Concat

(><) :: RegEx a -> RegEx a -> RegEx a
(><) = Sum

match :: [a] -> RegEx a
match = Prelude.foldr (Concat . Match) Epsilon

times :: Int -> RegEx a -> RegEx a
times n r = Prelude.foldr Concat Epsilon $ replicate n r

upto :: Int -> RegEx a -> RegEx a
upto n r = Prelude.foldr Sum Empty [times t r | t <- [0..n]]

atleast :: Int -> RegEx a -> RegEx a
atleast n r =  times n r <> many r

bounded :: Int -> Int -> RegEx a -> RegEx a
bounded start end regex = times start regex <> upto (end - start) regex

one :: RegEx Char
one = char '1'

zero :: RegEx Char
zero = char '0'

any :: (Foldable f) => f (RegEx a) -> RegEx a
any = Prelude.foldr Sum Empty