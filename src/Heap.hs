{-# LANGUAGE FlexibleInstances #-}
module Heap where

import Test.QuickCheck
import ADT.Queue

instance Queue Heap where
  empty = E
  snoc = flip insert
  head = findMin
  tail = deleteMin

data Heap a = E | T Int a (Heap a) (Heap a)
  deriving (Show, Eq)

instance Arbitrary (Heap Int) where
  arbitrary = arbHeap 0 1000

arbHeap :: Int -> Int -> Gen (Heap Int)
arbHeap lo hi = fromList <$> listOf (choose (lo, hi))

empty :: Heap a
empty = E

merge :: Ord a => Heap a -> Heap a -> Heap a
merge h E = h
merge E h = h
merge h1@(T _ x a1 b1) h2@(T _ y a2 b2)
  | x <= y    = makeT x a1 (merge b1 h2)
  | otherwise = makeT y a2 (merge h1 b2)

rank :: Heap a -> Int
rank E = 0
rank (T r _ _ _) = r

makeT :: a -> Heap a -> Heap a -> Heap a
makeT x a b
  | rank a >= rank b = T (rank b + 1) x a b
  | otherwise = T (rank a + 1) x b a

insert :: Ord a => a -> Heap a -> Heap a
insert x = merge (T 1 x E E)

insert' :: Ord a => a -> Heap a -> Heap a
insert' a E = T 1 a E E
insert' a (T r x t1 t2)
  | a >= x =
    let t2' = insert' a t2
    in if rank t1 < rank t2'
      then T (rank t1 + 1) x t2' t1
      else T (rank t2' + 1) x t1 t2'
  | otherwise =
    let t2' = insert' x t2
    in if rank t1 < rank t2'
      then T (rank t1 + 1) a t2' t1
      else T (rank t2' + 1) a t1 t2'

findMin :: Heap a -> Maybe a
findMin E = Nothing
findMin (T _ x _ _) = Just x

deleteMin :: Ord a => Heap a -> Heap a
deleteMin E = E
deleteMin (T _ _ a b) = merge a b

fromList :: Ord a => [a] -> Heap a
fromList [] = E
fromList [a] = T 1 a E E
fromList [a, b] = merge (fromList [a]) (fromList [b])
fromList (x:xs) =
  let left = fromList [a | a <- xs, a <= x]
      right = fromList [a | a <- xs, a > x]
  in merge left right

data WHeap a = WE | WT a (WHeap a) (WHeap a)
  deriving (Show, Eq)

emptyW :: WHeap a
emptyW = WE

mergeW :: Ord a => WHeap a -> WHeap a -> WHeap a
mergeW h WE = h
mergeW WE h = h
mergeW h1@(WT x a1 b1) h2@(WT y a2 b2)
  | x <= y    = makeTW x a1 (mergeW b1 h2)
  | otherwise = makeTW y a2 (mergeW h1 b2)

compareW :: Ord a => WHeap a -> WHeap a -> Bool
compareW _ WE = True
compareW WE _ = False
compareW (WT x _ _) (WT y _ _) = x >= y

makeTW :: Ord a => a -> WHeap a -> WHeap a -> WHeap a
makeTW x a b
  | compareW a b = WT x a b
  | otherwise = WT x b a

insertW :: Ord a => a -> WHeap a -> WHeap a
insertW x = mergeW (WT x WE WE)
