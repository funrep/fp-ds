module TreeSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.QuickCheck

import Tree

tests = testGroup "Unit tests" 
  [ testProperty "insert n . insert n == insert n" $
      \s n -> insert (n :: Int) (insert (n :: Int) (s :: Set Int)) == insert (n :: Int) (s :: Set Int)
  , testProperty "member n . insert n == True" $
      \s n -> member (n :: Int) (insert (n :: Int) (s :: Set Int))
  -- , testProperty "height . complete k d == d" $
  --   \k d -> 
  --     if (d :: Int) < 0
  --       then True
  --       else height (complete (k :: Int) (d :: Int)) == (d :: Int)
  -- , testProperty "size . balanced k m == m" $
  --     \k m -> fmap (\n -> size (balanced (k :: Int) n) == n) (m :: Positive m) == Positive True
  ]
