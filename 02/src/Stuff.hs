module Stuff
  ( group
  , sortBy
  , groupBy
  , sortOn
  , groupOn
  , classifyOn
  , (&&&)
  , on
  ) where

import Data.List (partition)

group :: Eq a => [a] -> [[a]]
group [] = []
group xs = go xs []
 where
   go [] grouped = grouped
   go (y:ys) grouped = go (dropWhile (==y) ys) (grouped ++ [y : takeWhile (==y) ys])

sortBy :: (a -> a -> Ordering) -> [a] -> [a]
sortBy _ [] = []
sortBy comparator (pivot:others) = sortBy comparator lessThanOrEqual ++ (pivot : sortBy comparator greater)
 where
   (lessThanOrEqual, greater) = partition (\el -> comparator el pivot /= GT) others

groupBy :: (a -> a -> Bool) -> [a] -> [[a]]
groupBy _ [] = []
groupBy f xs = go f xs []
 where
   go _ [] grouped = grouped
   go g (y:ys) grouped = go g (dropWhile (g y) ys) (grouped ++ [y : takeWhile (g y) ys])

on :: (b -> b -> c) -> (a -> b) -> a -> a -> c
on b u x y = b (u x) (u y)

(&&&) :: (a -> b) -> (a -> c) -> a -> (b, c)
(&&&) toB toC a = (toB a, toC a)

sortOn :: Ord b => (a -> b) -> [a] -> [a]
sortOn f = sortBy (compare `on` f)

groupOn :: Eq b => (a -> b) -> [a] -> [[a]]
groupOn f = groupBy ((==) `on` f)

classifyOn :: Ord b => (a -> b) -> [a] -> [[a]]
classifyOn f xs = groupOn f (sortOn f xs)