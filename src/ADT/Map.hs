{-# LANGUAGE MultiParamTypeClasses #-}
module ADT.Map where

class Ord k => Map m k where
  empty :: m k a
  lookup :: k -> m k a -> Maybe a
  insert :: k -> a -> m k a -> m k a
