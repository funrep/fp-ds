{-# LANGUAGE FlexibleInstances #-}
module BinHeap where

import Test.QuickCheck
import ADT.Heap

instance Heap BinHeap where
  empty = BinHeap []
  insert a (BinHeap q) = BinHeap $ insertBT a q
  merge (BinHeap a) (BinHeap b) = BinHeap $ mergeBT a b
  findMin (BinHeap q) = findMinBT q
  deleteMin (BinHeap q) = BinHeap $ deleteMinBT q

data Tree a = Node Int a [Tree a]
  deriving (Show, Eq)

link :: Ord a => Tree a -> Tree a -> Tree a
link t1@(Node r x1 c1) t2@(Node _ x2 c2)
  | x1 <= x2  = Node (r + 1) x1 (t2 : c1)
  | otherwise = Node (r + 1) x2 (t1 : c2)

newtype BinHeap a = BinHeap (BinHeapT a)
  deriving (Show, Eq)

instance Arbitrary (BinHeap Int) where
  arbitrary = arbBinHeap 0 1000

arbBinHeap :: Int -> Int -> Gen (BinHeap Int)
arbBinHeap lo hi = do
  xs <- listOf $ choose (lo, hi)
  pure $ foldr insert empty xs

type BinHeapT a = [Tree a]

rank :: Tree a -> Int
rank (Node r _ _) = r

root :: Tree a -> a
root (Node _ a _) = a

insTree :: Ord a => Tree a -> BinHeapT a -> BinHeapT a
insTree x [] = [x]
insTree x (t:ts)
  | rank x < rank t = x : ts
  | otherwise = insTree (link x t) ts

insertBT :: Ord a => a -> BinHeapT a -> BinHeapT a
insertBT x = insTree (Node 0 x [])

mergeBT :: Ord a => BinHeapT a -> BinHeapT a -> BinHeapT a
mergeBT ts [] = ts
mergeBT [] ts = ts
mergeBT ts1@(x:xs) ts2@(y:ys)
  | rank x < rank y = x : mergeBT xs ts2
  | rank x > rank y = y : mergeBT ts1 ys
  | otherwise = insTree (link x y) (mergeBT xs ys)

removeMinTree :: Ord a => [Tree a] -> (Tree a, [Tree a])
removeMinTree [] = error "removeMinTree given empty list"
removeMinTree [t] = (t, [])
removeMinTree (t:ts) =
  let (x, xs) = removeMinTree ts
  in if root t <= root x
    then (t, ts)
    else (x, t : xs)

findMinBT :: Ord a => BinHeapT a -> Maybe a
findMinBT [] = Nothing
findMinBT [t] = Just $ root t
findMinBT (t:ts) = do
  x <- findMinBT ts
  if root t <= x then Just $ root t else Just x

deleteMinBT :: Ord a => BinHeapT a -> BinHeapT a
deleteMinBT ts =
  let (Node _ x xs, ys) = removeMinTree ts
  in mergeBT (reverse xs) ys
