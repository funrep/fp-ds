module List where

import ADT.Stack

instance Stack List where
  empty = Nil
  cons = Cons
  head = headS
  tail = tailS

data List a = Nil | Cons a (List a)
  deriving (Show, Eq)

empty :: List a
empty = Nil

isEmpty :: List a -> Bool
isEmpty Nil = True
isEmpty _ = False

cons :: a -> List a -> List a
cons = Cons

headS :: List a -> Maybe a
headS Nil = Nothing
headS (Cons a _) = Just a

tailS :: List a -> List a
tailS Nil = Nil
tailS (Cons _ t) = t

concatS :: List a -> List a -> List a
concatS Nil ys = ys
concatS (Cons x xs) ys = Cons x $ concatS xs ys

suffixes :: [a] -> [[a]]
suffixes [] = [[]]
suffixes l@(_:xs) = l : suffixes xs
-- O(n) time from iterating through the list once
-- O(n) space from every element sublist sharing memory with the initial list
