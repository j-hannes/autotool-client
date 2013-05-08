module Model.Types.Name where

data Name = Name
  { namePrename :: String
  , nameSurname :: String
  } deriving (Eq, Read, Show)
