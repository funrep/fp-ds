module TreeHeapSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.QuickCheck
import Data.Maybe

import TreeHeap
import qualified ADT.Heap as H

tests = testGroup "Heap with TreeHeap"
  [ testProperty "H.findMin <= H.findMin . H.deleteMin" $
      \q -> testOrTrue (<=) (H.findMin (q :: TreeHeap Int)) (H.findMin (H.deleteMin (q :: TreeHeap Int)))
  , testProperty "if H.findMin > n then H.findMin . H.insert n <= H.FindMin else H.findMin . H.insert n >= H.findMin" $
      \q n ->
        if testOrFalse (>) (H.findMin (q :: TreeHeap Int)) (Just (n :: Int))
          then testOrTrue (<=) (H.findMin $ H.insert (n :: Int) (q :: TreeHeap Int)) (H.findMin (q :: TreeHeap Int))
          else testOrTrue (>=) (H.findMin $ H.insert (n :: Int) (q :: TreeHeap Int)) (H.findMin (q :: TreeHeap Int))
  ]

testOrTrue :: (a -> a -> Bool) -> Maybe a -> Maybe a -> Bool
testOrTrue g a b = fromMaybe True res
  where
    res = do
      x <- a
      g x <$> b

testOrFalse :: (a -> a -> Bool) -> Maybe a -> Maybe a -> Bool
testOrFalse g a b = fromMaybe True res
  where
    res = do
      x <- a
      g x <$> b
