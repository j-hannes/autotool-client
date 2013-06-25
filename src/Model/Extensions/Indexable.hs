module Model.Extensions.Indexable (
    Indexable
  , iid
  , setId
  ) where

import Model.Datatypes

------------------------------------------------------------------------------
-- | Class to convert FileStore data into Haskell datatypes.
class Indexable a where
  iid :: a -> String
  setId :: a -> String -> a

------------------------------------------------------------------------------ 
-- | Instances for FileStore
instance Indexable Assignment where
  iid = assignmentId
  setId assn idVal = assn { assignmentId = idVal }

instance Indexable Course where
  iid = courseId
  setId assn idVal = assn { courseId = idVal }

instance Indexable Enrollment where
  iid = enrollmentId
  setId assn idVal = assn { enrollmentId = idVal }

instance Indexable Group where
  iid = groupId
  setId group idVal = group { groupId = idVal }

instance Indexable Solution where
  iid = solutionId
  setId solutionconfig idVal = solutionconfig { solutionId = idVal }

instance Indexable Student where
  iid = studentId
  setId assn idVal = assn { studentId = idVal }

instance Indexable Task where
  iid = taskId
  setId taskconfig idVal = taskconfig { taskId = idVal }

instance Indexable TaskInstance where
  iid = taskInstanceId
  setId assn idVal = assn { taskInstanceId = idVal }

instance Indexable Tutor where
  iid = tutorId
  setId assn idVal = assn { tutorId = idVal }

