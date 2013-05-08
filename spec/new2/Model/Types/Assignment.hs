module Model.Types.Assignment where

import Model.Types.Reference
import Model.Types.Status
import Model.Types.Timespan

data Assignment = Assignment
  { assignmentTask   :: Reference
  , assignmentCourse :: Reference
  , assignmentStatus :: Status
  , assignmentSubmissionTime :: Timespan
  } deriving (Eq, Read, Show)
