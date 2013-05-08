module Types.Name
  ( Name
  , newName
  , prename
  , surname
  ) where

data Name = N
  { prename :: String
  , surname :: String
  } deriving (Eq, Read, Show)

newName :: String -> String -> Name
newName = N
