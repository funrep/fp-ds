{-# LANGUAGE FlexibleInstances #-}
module Tree where

import Test.QuickCheck

data Tree a = Leaf | Node (Tree a) a (Tree a)
  deriving (Show, Eq)

instance Arbitrary (Tree Int) where
  arbitrary = arbBinTree 0 1000

arbBinTree :: Int -> Int -> Gen (Tree Int)
arbBinTree lo hi = oneof
  [ pure Leaf
  , do
      v <- choose (lo, hi)
      l <- arbBinTree lo (v - 1)
      r <- arbBinTree (v + 1) hi
      pure $ Node l v r
  ]

type Set a = Tree a

empty :: Set a
empty = Leaf

-- insert :: Ord a => a -> Set a -> Set a
-- insert a Leaf = Node Leaf a Leaf
-- insert a t@(Node t1 x t2)
--   | a < x = Node (insert a t1) x t2
--   | a > x = Node t1 x (insert a t2)
--   | otherwise = t

insert :: Ord a => a -> Set a -> Set a
insert a Leaf = Node Leaf a Leaf
insert a t@(Node t1 x t2)
  | a `member` t = t
  | a < x = Node (insert a t1) x t2
  | a > x = Node t1 x (insert a t2)
  | otherwise = t

-- member :: Ord a => a -> Set a -> Bool
-- member a Leaf = False
-- member a (Node t1 x t2) =
--   if a < x then member a t1
--   else if a > x then member a t2
--   else True

member :: Ord a => a -> Set a -> Bool
member a Leaf = False
member a t@(Node _ c _) = f c a t
  where
    f c a Leaf = c == a
    f c a (Node t1 x t2)
      | a <= x    = f x a t1
      | otherwise = f c a t2

size :: Tree a -> Int
size Leaf = 0
size (Node t1 _ t2) = 1 + size t1 + size t2

height :: Tree a -> Int
height Leaf = 0
height (Node t1 _ t2) = 1 + max (height t1) (height t2)

-- complete :: a -> Int -> Tree a
-- complete a d
--   | d <= 0 = Leaf
--   | otherwise =
--     let t = complete a (d - 1)
--     in Node t a t

-- balanced :: a -> Int -> Tree a
-- balanced _ 0 = Leaf
-- balanced a 1 = Node Leaf a Leaf
-- balanced a m =
--   let (t1, t2) = create2 a ((m - 1) `div` 2)
--   in Node t1 a t2

-- create2 :: a -> Int -> (Tree a, Tree a)
-- create2 a 0 = (Leaf, Node Leaf a Leaf)
-- create2 a m =
--   let d = floor $ sqrt $ fromIntegral m
--       t@(Node t1 _ _) = complete a d
--   in (t1, t)

type Map k a = Tree (k, a)

bind :: Ord k => k -> a -> Map k a -> Map k a
bind k a Leaf = Node Leaf (k, a) Leaf
bind k a t@(Node t1 (x, _) t2)
  | k == x = Node t1 (x, a) t2
  | k < x = bind k a t1
  | k > x = bind k a t2

lookupM :: Ord k => k -> Map k a -> Maybe a
lookupM _ Leaf = Nothing
lookupM k (Node t1 (x, a) t2)
  | k == x = Just a
  | k < x = lookupM k t1
  | k > x = lookupM k t2
