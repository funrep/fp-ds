module RedBlackTree where

data Color = R | B

data Tree a = E | T Color (Tree a) a (Tree a)

-- Invariant 1: No red node has a red child.
-- Invariant 2: Every path from the root to an empty node contains the same number of black nodes.

-- memberRBT :: Ord a => a -> Tree a -> Bool
-- memberRBT _ E = False
-- memberRBT a (T _ t1 x t2)
--   | a < x = memberRBT x t1
--   | a > x = memberRBT x t2
--   | otherwise = True

-- insertRBT :: Ord a => a -> Tree a -> Tree a
-- insertRBT a tree =
--   let (T _ t1 x t2) = f tree
--   in T B t1 x t2
--   where
--     f E = T R E a E
--     f t@(T c t1 x t2)
--       | a < x = balance c (f t1) x t2
--       | a > x = balance c t1 x (f t2)
--       | otherwise = t

-- balance :: Color -> Tree a -> a -> Tree a -> Tree a
-- balance B (T R (T R a x b) y c) z d   = T R (T B a x b) y (T B c z d)
-- balance B (T R a x (T R b y c)) z d = T R (T B a x b) y (T B c z d)
-- balance B a x (T R (T R b y c) z d)   = T R (T B a x b) y (T B c z d)
-- balance B a x (T R b y (T R c z d)) = T R (T B a x b) y (T B c z d)
-- balance c t1 x t2 = T c t1 x t2

-- fromOrdList :: [a] -> Tree a
-- fromOrdList [] = E
-- fromOrdList [x] = T B E x E
-- fromOrdList [x, y] = T B E x (T R E y E)
-- fromOrdList [x, y, z] = T B (T R E x E) y (T R E z E)
-- fromOrdList xs =
--   let mid = length xs `div` 2
--       midElem = xs !! mid
--       leftElems = [x | x <- xs, x < midElem]
--       rightElems = [x | x <- xs, x > midElem ]
--   in T 
