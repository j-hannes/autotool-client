module Model.Types.TaskInstance where

import Model.Types.Reference

data TaskInstance = TaskInstance
  { taskInstanceStudent    :: Reference
  , taskInstanceAssignment :: Reference
  , taskInstanceSignature  :: String
  } deriving (Eq, Read, Show)
