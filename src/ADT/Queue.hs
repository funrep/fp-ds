module ADT.Queue where

class Queue q where
  empty :: q a
  snoc :: q a -> a -> q a
  head :: q a -> Maybe a
  tail :: q a -> q a
