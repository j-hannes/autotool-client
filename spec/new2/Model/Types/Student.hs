module Model.Types.Student where

import Model.Types.Name

data Student = Student
  { studentName     :: Name
  , studentNumber   :: String
  , studentPassword :: String
  , studentEmail    :: String
  } deriving (Eq, Read, Show)
