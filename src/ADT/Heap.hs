module ADT.Heap where

class Heap h where
  empty :: h a
  insert :: Ord a => a -> h a -> h a
  merge :: Ord a => h a -> h a -> h a
  findMin :: Ord a => h a -> Maybe a
  deleteMin :: Ord a => h a -> h a
