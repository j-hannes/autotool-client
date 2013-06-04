module Model.Types where

import Data.Time (UTCTime)
import Autotool.Client.Types.ScoringOrder
import Model.Indexable


------------------------------------------------------------------------------
-- |
data Assignment = Assignment {
    -- ^ Identifier
    assignmentId       :: Integer
    -- ^ Relations
  , assignmentCourseId :: Integer
  , assignmentTaskId   :: Integer
    -- ^ Attributes
  , assignmentStatus   :: Status
  , assignmentStart    :: UTCTime
  , assignmentEnd      :: UTCTime
  } deriving (Eq, Read, Show)

data Status = Mandatory
            | Optional
            deriving (Read, Show, Eq)

instance Indexable Assignment where
  iid = assignmentId
  setId assn idVal = assn { assignmentId = idVal }


------------------------------------------------------------------------------
-- |
data Course = Course {
    -- ^ Identifier
    courseId             :: Integer
    -- ^ Relations
  , courseTutorId        :: Integer
    -- ^ Attributes
  , courseName           :: String
  , courseSemester       :: String
  , courseEnrollmentFrom :: Maybe UTCTime
  , courseEnrollmentTo   :: Maybe UTCTime
  , coursePassCriteria   :: Double
  } deriving (Eq, Ord, Read, Show)


instance Indexable Course where
  iid = courseId
  setId course idVal = course { courseId = idVal }


------------------------------------------------------------------------------
-- |
data Enrollment = Enrollment {
    -- ^ Identifier
    enrollmentId        :: Integer
    -- ^ Relations
  , enrollmentGroupId   :: Integer
  , enrollmentStudentId :: Integer
    -- ^ Attributes
  , enrollmentTime      :: UTCTime
  } deriving (Eq, Read, Show)

instance Indexable Enrollment where
  iid = enrollmentId
  setId assn idVal = assn { enrollmentId = idVal }


------------------------------------------------------------------------------
-- |
data Group = Group {
    -- ^ Identifier
    groupId             :: Integer
    -- ^ Relations
  , groupCourseId       :: Integer
    -- ^ Attributes
  , groupDescription    :: String
  , groupCapacity       :: Int
  } deriving (Eq, Ord, Read, Show)

instance Indexable Group where
  iid = groupId
  setId group idVal = group { groupId = idVal }


------------------------------------------------------------------------------
-- |
data Solution = Solution {
    -- ^ Identifier
      solutionId             :: Integer
    -- ^ Relations
    , solutionTaskInstanceId :: Integer
    -- ^ Attributes
    , solutionContent        :: String
    , solutionEvaluation     :: String
    , solutionResult         :: Maybe Result
    , solutionSubmission     :: UTCTime
    } deriving (Read, Show)

data Result = Result {
      score :: Int
    , size  :: Int
    } deriving (Eq, Read, Show)

instance Indexable Solution where
  iid = solutionId
  setId solutionconfig idVal = solutionconfig { solutionId = idVal }


------------------------------------------------------------------------------
-- |
data Task = Task {
    -- ^ Identifier
      taskId           :: Integer
    -- ^ Relations
    , taskTutorId      :: Integer
    -- ^ Attributes
    , taskName         :: String
    , taskType         :: String
    , taskSignature    :: String
    , taskScoringOrder :: ScoringOrder
    , taskCreated      :: UTCTime
    } deriving (Read, Show)


instance Indexable Task where
  iid = taskId
  setId taskconfig idVal = taskconfig { taskId = idVal }


------------------------------------------------------------------------------
-- |
data TaskInstance = TaskInstance {
    -- ^ Identifier
    taskInstanceId        :: Integer
    -- ^ Relations
  , taskInstanceTaskId    :: Integer
  , taskInstanceStudentId :: Integer
    -- ^ Attributes
  , taskInstanceDescription   :: String
  , taskInstanceDocumentation :: String
  , taskInstanceSolution      :: String
  , taskInstanceSignature     :: String
  } deriving (Eq, Read, Show)


instance Indexable TaskInstance where
  iid = taskInstanceId
  setId assn idVal = assn { taskInstanceId = idVal }


------------------------------------------------------------------------------
-- |
type CourseBundle     = (Course, [(Assignment, Task)])
type GroupBundle      = (Group, Course, [AssignmentBundle])
type AssignmentBundle = (Assignment, Task, TaskInstance)
