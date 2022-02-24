module List where

data Stack a = Nil | Cons a (Stack a)
  deriving (Show, Eq)

empty :: Stack a
empty = Nil

isEmpty :: Stack a -> Bool
isEmpty Nil = True
isEmpty _ = False

cons :: a -> Stack a -> Stack a
cons = Cons

headS :: Stack a -> Maybe a
headS Nil = Nothing
headS (Cons a _) = Just a

tailS :: Stack a -> Stack a
tailS Nil = Nil
tailS (Cons _ t) = t

concatS :: Stack a -> Stack a -> Stack a
concatS Nil ys = ys
concatS (Cons x xs) ys = Cons x $ concatS xs ys

suffixes :: [a] -> [[a]]
suffixes [] = []
suffixes l@(_:xs) = l : suffixes xs
-- O(n) time from iterating through the list once
-- O(n) space from every element sublist sharing memory with the initial list



