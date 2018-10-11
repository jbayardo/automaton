{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}

module CFG where

import Control.Lens as Lens hiding (children, to)
import Control.Monad.Cont
import Data.Either
import Data.Functor.Foldable
import Data.Functor.Foldable.TH
import Data.Map.Strict as Map
import Data.Maybe
import Data.Set as Set
import qualified Data.Traversable as T

type Symbol v t = Either v t

data Epsilon =
  Epsilon
  deriving (Show)

data ParseTree v t
  = Interior { _label :: v
             , _children :: Either Epsilon [ParseTree v t] }
  | Leaf { _value :: Symbol v t }
  deriving (Show)

makeLenses ''ParseTree

makeBaseFunctor ''ParseTree

data CFG v t = CFG
  { _initial :: v
  , _rules :: Map v [ParseTree v t]
  }

makeLenses ''CFG

cfg :: (Ord v) => v -> [ParseTree v t] -> CFG v t
cfg initial rules' =
  CFG {_initial = initial, _rules = Map.fromListWith (++) rules}
    -- TODO: check that all elements are Interior nodes!, and are no more than 1 level deep.
  where
    heads = fmap _label rules'
    rules = zip heads $ fmap (: []) rules'

to :: v -> [Symbol v t] -> ParseTree v t
to variable symbols =
  Interior
    { _label = variable
    , _children =
        case symbols of
          [] -> Left Epsilon
          _ -> Right $ fmap Leaf symbols
    }

start :: CFG v t -> ParseTree v t
start cfg = Leaf {_value = Left (cfg ^. initial)}

expand :: (Ord v) => CFG v t -> ParseTree v t -> Maybe [ParseTree v t]
-- Leaf nodes may be expanded if they are variables. All we have to do is check
expand cfg tree@Leaf {_value = value} =
  case value of
    Left variable -> Map.lookup variable (cfg ^. rules)
    Right _ -> Nothing
-- Interior nodes have already been expanded. There's nothing we can possibly
-- do with these
expand _ _ = Nothing

expandable :: (Ord v) => ParseTree v t -> Bool
expandable = cata go
  where
    go LeafF {_valueF = symbol} = isLeft symbol
    go InteriorF {_childrenF = children} = either (const False) or children

represented :: ParseTree v t -> [Symbol v t]
represented = cata go
  where
    go LeafF {_valueF = symbol} = [symbol]
    go InteriorF {_childrenF = children} = either (const []) concat children

-- Lo que quiero hacer es ver a una variable como una continuación que aun no
-- fue expandida. Entonces, lo que determina un parse tree es el orden de 
-- evaluación de las continuaciones.
example =
  'S' `cfg`
  ['S' `to` [Right 'a', Left 'S'], 'S' `to` [Right 'b', Left 'S'], 'S' `to` []]

_first1' ::
     (Ord v, Ord t) => CFG v t -> [Symbol v t] -> ListF (Set [t]) [Symbol v t]
-- No symbols. Easy!.
_first1' _ [] = Nil
-- If this is the last symbol, don't mind if there's any epsilon.
_first1' cfg [value] = Cons (first1 cfg [value]) []
-- There is more than one symbol. In this case, we care about whether the
-- head could possibly be epsilon or not. If it could, then we need to look
-- at the next symbol to decide what the first set is for this rule. 
_first1' cfg (value:cs) =
  let first = first1 cfg [value]
   in if [] `Set.member` first
        then Cons (Set.delete [] first) cs
        else Cons first []

-- WARNING: this function hangs if the grammar is left recursive
-- WARNING: this function returns a set of lists, but in the case of FIRST_1,
-- the lists are either empty (epsilon), or have a single terminal.
first1 ::
     forall v t. (Ord v, Ord t)
  => CFG v t
  -> [Symbol v t]
  -> Set [t]
-- Empty string, empty FIRST_1 set.
first1 cfg [] = Set.singleton []
-- Just a terminal. Only one possible option.
first1 cfg [Right terminal] = Set.singleton [terminal]
-- A variable, we have to look into the rules we have for it.
first1 cfg [Left variable] =
  Set.unions $ first1' <$> Map.findWithDefault [] variable (cfg ^. rules)
  where
    first1' Interior {_label = label, _children = children} =
      case children of
        Left Epsilon -> Set.singleton []
        Right cs -> Set.unions $ ana (_first1' cfg) $ _value <$> cs
-- A string, this is more complicated.
first1 cfg alpha = Set.unions $ ana (_first1' cfg) alpha

data EOF = EOF

follow1 :: (Ord v, Ord t) => CFG v t -> v -> Set (Either EOF t)
follow1 cfg variable = undefined

-- http://okmij.org/ftp/Haskell/ZipperTraversable.hs
data Zipper t a
  = ZDone (t a)
  | Z a
      (Maybe a -> Zipper t a)

makeZipper :: T.Traversable t => t a -> Zipper t a
makeZipper t = reset $ ZDone <$> T.mapM f t
  where
    f a = shift (\k -> return $ Z a (k . fromMaybe a))

zipUp :: Zipper t a -> t a
zipUp (ZDone t) = t
zipUp (Z _ k) = zipUp $ k Nothing

extract :: Zipper t a -> a
extract (Z a _) = a

next :: Zipper t a -> Zipper t a
next (Z a f) = f Nothing
next x = x

-- Two delimited control operators,
-- without answer-type polymorphism or modification
-- These features are not needed for the application at hand
reset :: Cont r r -> r
reset m = runCont m id

shift :: ((a -> r) -> Cont r r) -> Cont r a
shift e = cont (reset . e)
