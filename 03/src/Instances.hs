{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -fplugin=HLint #-} -- run hlint on build via the hlint source plugin

module Instances where

import Prelude hiding (reverse)

import Data.Char (isSpace)
import Data.Function (on)

newtype Pointwise a b = Pointwise {getPointwise :: (a, b)}
  deriving (Show, Eq)

instance (Ord a, Ord b) => Ord (Pointwise a b) where
  (<=) :: Pointwise a b -> Pointwise a b -> Bool
  Pointwise (p1a, p1b) <= Pointwise (p2a, p2b) = p1a <= p2a && p1b <= p2b

newtype Lexicographic a b = Lexicographic {getLexicographic :: (a, b)}
  deriving (Show, Eq)

-- The default instance for tuples and lists
instance (Ord a, Ord b) => Ord (Lexicographic a b) where
  (<=) :: Lexicographic a b -> Lexicographic a b -> Bool
  Lexicographic (l1a, l1b) <= Lexicographic (l2a, l2b) = l1a < l2a || (l1a == l2a && l1b <= l2b)

newtype Fun a b = Fun {getFun :: a -> b}

instance (Semigroup b) => Semigroup (Fun a b) where
  (<>) :: Fun a b -> Fun a b -> Fun a b
  (Fun f) <> (Fun g) = Fun (\x -> f x <> g x)

instance (Monoid b) => Monoid (Fun a b) where
  mempty :: Fun a b
  mempty = Fun mempty

newtype First a = First {getFirst :: Maybe a}
  deriving (Eq, Show)

instance Semigroup (First a) where
  (<>) :: First a -> First a -> First a
  First Nothing <> b = b
  a <> _ = a

instance Monoid (First a) where
  mempty :: First a
  mempty = First Nothing

newtype Last a = Last {getLast :: Maybe a}
  deriving (Eq, Show)

instance Semigroup (Last a) where
  (<>) :: Last a -> Last a -> Last a
  a <> Last Nothing = a
  _ <> b = b

instance Monoid (Last a) where
  mempty :: Last a
  mempty = Last Nothing

newtype Pair a b = Pair {getPair :: (a, b)}
  deriving (Show, Eq)

-- The default instance for tuples
instance (Semigroup a, Semigroup b) => Semigroup (Pair a b) where
  (<>) :: Pair a b -> Pair a b -> Pair a b
  Pair (p1a, p1b) <> Pair (p2a, p2b) = Pair (p1a <> p2a, p1b <> p2b)

instance (Monoid a, Monoid b) => Monoid (Pair a b) where
  mempty :: Pair a b
  mempty = Pair (mempty, mempty)

newtype Dual a = Dual {getDual :: a}
  deriving (Show, Eq)

instance Semigroup a => Semigroup (Dual a) where
  (<>) :: Dual a -> Dual a -> Dual a
  Dual a <> Dual b = Dual (b <> a)

instance Monoid a => Monoid (Dual a) where
  mempty :: Dual a
  mempty = Dual mempty

reverse :: [a] -> [a]
reverse = foldr (\x xs -> getDual (Dual [x] <> Dual xs)) [] -- couldn't make it worse :D

data Flux a = Flux
  { sides :: Maybe (a, a)
  , changes :: Int
  }
  deriving (Show, Eq)

flux :: a -> Flux a
flux x = Flux (Just (x, x)) 0

instance (Eq a) => Semigroup (Flux a) where
  (<>) :: Flux a -> Flux a -> Flux a
  Flux Nothing _ <> g = g
  f <> Flux Nothing _ = f
  Flux (Just (j1a, j1b)) j1n <> Flux (Just (j2a, j2b)) j2n =
    Flux (Just (j1a, j2b)) (j1n + j2n + (if j1b /= j2a then 1 else 0))

instance (Eq a) => Monoid (Flux a) where
  mempty :: Flux a
  mempty = Flux Nothing 0
