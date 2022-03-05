{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Tree where

import Test.QuickCheck
import ADT.Set
import ADT.Map

instance Set Tree where
  empty = Leaf
  member = memberT
  insert = insertT

newtype TreeMap k a = TreeMap (Tree (k, a))
  deriving (Eq, Show)

instance Ord k => Map TreeMap k where
  empty = TreeMap Leaf
  lookup k (TreeMap m) = lookupM k m
  insert k a (TreeMap m) = TreeMap $ bind k a m

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

instance Arbitrary (TreeMap Int Int) where
  arbitrary = arbBinTreeMap 0 1000

arbBinTreeMap :: Int -> Int -> Gen (TreeMap Int Int)
arbBinTreeMap lo hi = oneof
  [ pure $ TreeMap Leaf
  , do
      v <- choose (lo, hi)
      x <- arbitrary
      TreeMap l <- arbBinTreeMap lo (v - 1)
      TreeMap r <- arbBinTreeMap (v + 1) hi
      pure $ TreeMap $ Node l (v, x)  r
  ]

emptyT :: Tree a
emptyT = Leaf

-- insert :: Ord a => a -> Tree a -> Tree a
-- insert a Leaf = Node Leaf a Leaf
-- insert a t@(Node t1 x t2)
--   | a < x = Node (insert a t1) x t2
--   | a > x = Node t1 x (insert a t2)
--   | otherwise = t

insertT :: Ord a => a -> Tree a -> Tree a
insertT a Leaf = Node Leaf a Leaf
insertT a t@(Node t1 x t2)
  | a < x = f a t t1
  | a > x = f a t t2
  | otherwise = t
  where
    f a Leaf Leaf = Node Leaf a Leaf
    f a p@(Node t1 x t2) Leaf
      | a < x = Node (Node Leaf a Leaf) x t2
      | a > x = Node t1 x (Node Leaf a Leaf)
      | otherwise = Leaf
    f a p t@(Node t1 x t2)
      | a < x = f a t t1
      | a > x = f a t t2
      | otherwise = p

-- member :: Ord a => a -> Tree a -> Bool
-- member a Leaf = False
-- member a (Node t1 x t2) =
--   if a < x then member a t1
--   else if a > x then member a t2
--   else True

-- Worst case O(d + 1)
-- Best case O(d + 1)
memberT :: Ord a => a -> Tree a -> Bool
memberT a Leaf = False
memberT a t@(Node _ c _) = f c a t
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

complete :: a -> Int -> Tree a
complete a d
  | d <= 0 = Leaf
  | otherwise =
    let t = complete a (d - 1)
    in Node t a t

balanced :: a -> Int -> Tree a
balanced _ 0 = Leaf
balanced a 1 = Node Leaf a Leaf
balanced a m =
  let (t1, t2) = create2 a ((m - 1) `div` 2)
  in Node t1 a t2

create2 :: a -> Int -> (Tree a, Tree a)
create2 a 0 = (Leaf, Node Leaf a Leaf)
create2 a m =
  let d1 = floor $ logBase 2 $ fromIntegral m
      d2 = floor $ logBase 2 $ fromIntegral $ m + 1
      t1 = complete a d1
      t2 = complete a d2
  in (t1, t2)

bind :: Ord k => k -> a -> Tree (k, a) -> Tree (k, a)
bind k a Leaf = Node Leaf (k, a) Leaf
bind k a t@(Node t1 (x, _) t2)
  | k == x = Node t1 (x, a) t2
  | k < x = bind k a t1
  | k > x = bind k a t2

lookupM :: Ord k => k -> Tree (k, a) -> Maybe a
lookupM _ Leaf = Nothing
lookupM k (Node t1 (x, a) t2)
  | k == x = Just a
  | k < x = lookupM k t1
  | k > x = lookupM k t2
