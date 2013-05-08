module Model.Indexable where

class Indexable a where
  iid :: a -> Integer
  setId :: a -> Integer -> a
