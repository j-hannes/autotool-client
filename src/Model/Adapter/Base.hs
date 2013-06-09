module Model.Adapter.Base
  ( getNextId
  ) where

import           Data.Map        (Map)
import qualified Data.Map        as Map

import           Model.Indexable

------------------------------------------------------------------------------
-- | Return the highest found index (id) + 1 from a list of indexable DTs.
getNextId :: (Indexable a) => Map Integer a -> Integer
getNextId m | Map.null m = 1
            | otherwise  = maximum (Map.keys m) + 1
