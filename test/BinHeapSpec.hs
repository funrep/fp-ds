module BinHeapSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.QuickCheck
import Data.Maybe

import BinHeap
import qualified ADT.Heap as H

tests = testGroup "Heap with BinHeap"
  [ testProperty "H.findMin <= H.findMin . H.deleteMin" $
      \q -> testOrTrue (<=) (H.findMin (q :: BinHeap Int)) (H.findMin (H.deleteMin (q :: BinHeap Int)))
  ]

testOrTrue :: (a -> a -> Bool) -> Maybe a -> Maybe a -> Bool
testOrTrue g a b = fromMaybe True res
  where
    res = do
      x <- a
      g x <$> b
