module TreeSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.QuickCheck
import Data.Maybe

import Tree
import qualified ADT.Set as S
import qualified ADT.Map as M

tests = testGroup "Set and map tests with trees"
  [ testProperty "S.insert n . S.insert n == S.insert n" $
      \s n -> S.insert (n :: Int) (S.insert (n :: Int) (s :: Tree Int)) == S.insert (n :: Int) (s :: Tree Int)
  , testProperty "S.member n . S.insert n == True" $
      \s n -> S.member (n :: Int) (S.insert (n :: Int) (s :: Tree Int))
  , testProperty "M.insert k n . M.insert k n == M.insert k n" $
      \m k n -> M.insert (k :: Int) (n :: Int) (M.insert (k :: Int) (n :: Int) (m :: TreeMap Int Int)) == M.insert (k :: Int) (n :: Int) (m :: TreeMap Int Int)
  , testProperty "isJust $ M.lookup k $ M.insert k n == True" $
      \m k n -> isJust $ M.lookup (k :: Int) $ M.insert (k :: Int) (n :: Int) (m :: TreeMap Int Int)
  ]
