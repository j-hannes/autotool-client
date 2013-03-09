module Model.Indexable where

class Indexable a where
  iid :: a -> Maybe Int
  setId :: a -> Int -> a
