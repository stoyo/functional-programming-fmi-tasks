{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -fplugin=HLint #-} -- run hlint on build via the hlint source plugin

module Trees where

import Prelude
import Data.Monoid (Sum(..), All(..), Any(..), First(..))
import Data.Maybe (isJust)

data Tree a
  = Empty
  | Node a (Tree a) (Tree a)
  deriving Show

instance Eq a => Eq (Tree a) where
  (==) :: Tree a -> Tree a -> Bool
  Empty == Empty = True
  Empty == _ = False
  _ == Empty = False
  (Node t1Val t1Left t1Right) == (Node t2Val t2Left t2Right) = t1Left == t2Left && t1Val == t2Val && t1Right == t2Right

insertOrdered :: Ord a => a -> Tree a -> Tree a
insertOrdered newVal Empty = Node newVal Empty Empty
insertOrdered newVal (Node rootVal t1 t2)
  | newVal > rootVal = Node rootVal t1 (insertOrdered newVal t2)
  | otherwise = Node rootVal (insertOrdered newVal t1) t2

listToBST :: Ord a => [a] -> Tree a
listToBST xs = go xs Empty
  where
    go ys tree = foldl (flip insertOrdered) tree ys

isBST :: Ord a => Tree a -> Bool
isBST Empty = True
isBST (Node _ Empty Empty) = True
isBST (Node val left@(Node leftVal _ _) Empty)
  | val >= leftVal = isBST left
  | otherwise = False
isBST (Node val Empty right@(Node rightVal _ _))
  | val < rightVal = isBST right
  | otherwise = False
isBST (Node val left@(Node leftVal _ _) right@(Node rightVal _ _))
  | val >= leftVal && val < rightVal = isBST left && isBST right
  | otherwise = False

findBST :: Ord a => a -> Tree a -> Bool
findBST _ Empty = False
findBST target (Node root left right)
  | target == root = True
  | target < root = findBST target left
  | otherwise = findBST target right

mapTree :: (a -> b) -> Tree a -> Tree b
mapTree _ Empty = Empty
mapTree f (Node val left right) = Node (f val) (mapTree f left) (mapTree f right)

foldTree :: Monoid a => Tree a -> a
foldTree Empty = mempty
foldTree (Node val left right) = foldTree left <> val <> foldTree right

foldMapTree :: Monoid b => (a -> b) -> Tree a -> b
foldMapTree f = foldTree . mapTree f

sumTree :: Num a => Tree a -> a
sumTree = getSum . foldMapTree Sum

allTree :: (a -> Bool) -> Tree a -> Bool
allTree p = getAll . foldMapTree (All . p)

treeToList :: Tree a -> [a]
treeToList = foldMapTree (: [])

elemTree :: Eq a => a -> Tree a -> Bool
elemTree val = getAny . foldMapTree (Any . (== val))

onMaybe :: (a -> Bool) -> a -> Maybe a
onMaybe p x = if p x then Just x else Nothing

findPred :: (a -> Bool) -> Tree a -> Maybe a
findPred p tree = getFirst (foldMapTree (First . onMaybe p) tree)

findAll :: (a -> Bool) -> Tree a -> [a]
findAll p = foldMapTree (\a -> [a | p a])

ifJust :: Maybe a -> (a -> Maybe b) -> Maybe b
ifJust Nothing _ = Nothing
ifJust (Just x) f = f x

validateTree :: (a -> Maybe b) -> Tree a -> Maybe (Tree b)
validateTree _ Empty = Just Empty
validateTree f (Node x l r) =
  ifJust (validateTree f l) (\l' -> ifJust (f x) (\x' -> ifJust (validateTree f r) (Just . Node x' l')))

data Direction
  = L -- go left
  | R -- go right
  deriving (Show, Eq)

fetch :: [Direction] -> Tree a -> Maybe a
fetch [] Empty = Nothing
fetch [] (Node val _ _) = Just val
fetch (_:_) Empty = Nothing
fetch (dir:dirs) (Node _ left right) =
  case dir of
    L -> fetch dirs left
    R -> fetch dirs right

mapDirections :: Tree a -> Tree (a, [Direction])
mapDirections Empty = Empty
mapDirections (Node v l r) = Node (v, []) (go [] L l) (go [] R r)
  where
    go :: [Direction] -> Direction -> Tree a -> Tree (a, [Direction])
    go _ _ Empty = Empty
    go dirs dir (Node value left right) =
      let dirsOnLevel = dirs ++ [dir]
      in Node (value, dirsOnLevel) (go dirsOnLevel L left) (go dirsOnLevel R right)

paths :: Tree a -> [(a, [Direction])]
paths = treeToList . mapDirections
