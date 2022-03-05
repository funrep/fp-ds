{-# LANGUAGE FlexibleInstances #-}
module TreeHeap where

import Test.QuickCheck
import ADT.Heap

instance Heap TreeHeap where
  empty = E
  insert = insertT
  merge = mergeT
  findMin = findMinT
  deleteMin = deleteMinT

data TreeHeap a = E | T Int a (TreeHeap a) (TreeHeap a)
  deriving (Show, Eq)

instance Arbitrary (TreeHeap Int) where
  arbitrary = arbTreeHeap 0 1000

arbTreeHeap :: Int -> Int -> Gen (TreeHeap Int)
arbTreeHeap lo hi = fromList <$> listOf (choose (lo, hi))

empty :: TreeHeap a
empty = E

mergeT :: Ord a => TreeHeap a -> TreeHeap a -> TreeHeap a
mergeT h E = h
mergeT E h = h
mergeT h1@(T _ x a1 b1) h2@(T _ y a2 b2)
  | x <= y    = makeT x a1 (mergeT b1 h2)
  | otherwise = makeT y a2 (mergeT h1 b2)

rank :: TreeHeap a -> Int
rank E = 0
rank (T r _ _ _) = r

makeT :: a -> TreeHeap a -> TreeHeap a -> TreeHeap a
makeT x a b
  | rank a >= rank b = T (rank b + 1) x a b
  | otherwise = T (rank a + 1) x b a

insertT :: Ord a => a -> TreeHeap a -> TreeHeap a
insertT x = merge (T 1 x E E)

insert' :: Ord a => a -> TreeHeap a -> TreeHeap a
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

findMinT :: TreeHeap a -> Maybe a
findMinT E = Nothing
findMinT (T _ x _ _) = Just x

deleteMinT :: Ord a => TreeHeap a -> TreeHeap a
deleteMinT E = E
deleteMinT (T _ _ a b) = merge a b

fromList :: Ord a => [a] -> TreeHeap a
fromList [] = E
fromList [a] = T 1 a E E
fromList [a, b] = merge (fromList [a]) (fromList [b])
fromList (x:xs) =
  let left = fromList [a | a <- xs, a <= x]
      right = fromList [a | a <- xs, a > x]
  in merge left right

data WTreeHeap a = WE | WT a (WTreeHeap a) (WTreeHeap a)
  deriving (Show, Eq)

emptyW :: WTreeHeap a
emptyW = WE

mergeW :: Ord a => WTreeHeap a -> WTreeHeap a -> WTreeHeap a
mergeW h WE = h
mergeW WE h = h
mergeW h1@(WT x a1 b1) h2@(WT y a2 b2)
  | x <= y    = makeTW x a1 (mergeW b1 h2)
  | otherwise = makeTW y a2 (mergeW h1 b2)

compareW :: Ord a => WTreeHeap a -> WTreeHeap a -> Bool
compareW _ WE = True
compareW WE _ = False
compareW (WT x _ _) (WT y _ _) = x >= y

makeTW :: Ord a => a -> WTreeHeap a -> WTreeHeap a -> WTreeHeap a
makeTW x a b
  | compareW a b = WT x a b
  | otherwise = WT x b a

insertW :: Ord a => a -> WTreeHeap a -> WTreeHeap a
insertW x = mergeW (WT x WE WE)
