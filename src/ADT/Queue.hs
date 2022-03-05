module ADT.Queue where

class Queue q where
  empty :: q a
  snoc :: Ord a => q a -> a -> q a
  head :: Ord a => q a -> Maybe a
  tail :: Ord a => q a -> q a
