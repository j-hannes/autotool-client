module Model.Types.Group where

import Model.Types.Reference

data Group = Group
  { groupcourse   :: Reference
  , groupName     :: String
  , groupCapacity :: Int
  } deriving (Eq, Read, Show)
