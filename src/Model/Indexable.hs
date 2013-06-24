module Model.Indexable where

class Indexable a where
  iid :: a -> String
  setId :: a -> String -> a
