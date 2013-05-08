module Model.Types.Task where

import Model.Types.Reference
import Model.Types.ScoringOrder

data Task = Task
  { taskTutor        :: Reference
  , taskName         :: String
  , taskTaskName     :: String
  , taskScoringOrder :: ScoringOrder
  , taskSignature    :: String
  } deriving (Eq, Read, Show)
