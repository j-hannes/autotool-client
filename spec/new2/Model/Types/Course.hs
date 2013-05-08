module Model.Types.Course where

import Model.Types.Reference
import Model.Types.Semester
import Model.Types.Timespan

data Course = Course
  { courseTutor          :: Reference
  , courseName           :: String
  , courseSemester       :: Semester
  , courseEnrollmentTime :: Timespan
  } deriving (Eq, Read, Show)
