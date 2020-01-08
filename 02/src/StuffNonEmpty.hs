module StuffNonEmpty
  ( NonEmpty(..)
  , mapNonEmpty
  , groupNonEmpty
  , groupByNonEmpty
  , groupOnNonEmpty
  , classifyOnNonEmpty
  ) where

import Stuff (sortOn, sortBy, on, (&&&))

groupNonEmpty :: Eq a => [a] -> [NonEmpty a]
groupNonEmpty [] = []
groupNonEmpty xs = go xs []
                   where
                     go [] grouped = grouped
                     go (y:ys) grouped = go (dropWhile (==y) ys) (grouped ++ [y :| takeWhile (==y) ys]) 

data NonEmpty a = a :| [a]
  deriving (Show, Eq, Ord)
infixr 4 :|

mapNonEmpty :: (a -> b) -> NonEmpty a -> NonEmpty b
mapNonEmpty f (a :| as) = f a :| map f as

groupByNonEmpty :: (a -> a -> Bool) -> [a] -> [NonEmpty a]
groupByNonEmpty _ [] = []
groupByNonEmpty f xs = go f xs []
                       where
                         go _ [] grouped = grouped
                         go g (y:ys) grouped = go g (dropWhile (g y) ys) (grouped ++ [y :| takeWhile (g y) ys])

groupOnNonEmpty :: Eq b => (a -> b) -> [a] -> [NonEmpty a]
groupOnNonEmpty f = groupByNonEmpty ((==) `on` f)

classifyOnNonEmpty :: Ord b => (a -> b) -> [a] -> [NonEmpty a]
classifyOnNonEmpty f xs = groupOnNonEmpty f (sortOn f xs)
