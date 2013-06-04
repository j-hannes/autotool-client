module Model.Adapter.Base
  ( getNextId
  ) where

import Model.Indexable

------------------------------------------------------------------------------
-- | Return the highest found index (id) + 1 from a list of indexable DTs.
getNextId :: (Indexable a) => Integer -> [a] -> Integer
getNextId n [] = n + 1
getNextId n (x:xs) | i > n     = getNextId i xs
                   | otherwise = getNextId n xs        
                   where
                     i = iid x
