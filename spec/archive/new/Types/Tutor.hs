module Types.Tutor
  ( Tutor
  , newTutor
  , name
  ) where

import Types.Name (Name, newName)

newtype Tutor = T
  { name :: Name
  } deriving (Eq, Show, Read)

newTutor :: String -> String -> Tutor
newTutor prename surname = T (newName prename surname)
