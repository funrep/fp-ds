module ADT.Stack where

class Stack s where
  empty :: s a
  cons :: a -> s a -> s a
  head :: s a -> Maybe a
  tail :: s a -> s a