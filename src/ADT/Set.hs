module ADT.Set where

class Set s where
  empty :: s a
  member :: Ord a => a -> s a -> Bool
  insert :: Ord a => a -> s a -> s a
