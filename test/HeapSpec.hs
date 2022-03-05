module HeapSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.QuickCheck
import Data.Maybe

import Heap
import qualified ADT.Queue as Q

tests = testGroup "Queue tests"
  [ testProperty "Q.head <= Q.head . Q.tail" $
      \q -> testOrTrue (<=) (Q.head (q :: Heap Int)) (Q.head (Q.tail (q :: Heap Int)))
  ]

testOrTrue :: (a -> a -> Bool) -> Maybe a -> Maybe a -> Bool
testOrTrue g a b = fromMaybe True res
  where
    res = do
      x <- a
      g x <$> b
