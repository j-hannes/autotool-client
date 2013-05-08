-------------------------------------------------------------------------------
-- TYPES
-------------------------------------------------------------------------------

module Types where

import Data.Time (UTCTime)
import Autotool.Client.Types.ScoringOrder (ScoringOrder)


-------------------------------------------------------------------------------
-- COMMON
-------------------------------------------------------------------------------

data FullName = FullName
  { prename :: String
  , surname :: String
  } deriving (Eq, Read, Show)

data TimeSpan = TimeSpan
  { from :: UTCTime
  , to   :: UTCTime
  } deriving (Eq, Read, Show)

type ServerHash = String


-------------------------------------------------------------------------------
-- TUTOR
-------------------------------------------------------------------------------

data Tutor = Tutor {
    -- ^ Identifier
    tutorid       :: Integer

    -- ^ Attributes
  , tutorName     :: FullName
  , tutorEmail    :: String
  , tutorPassword :: String
  } deriving (Eq, Read, Show)


-------------------------------------------------------------------------------
-- COURSE
-------------------------------------------------------------------------------

data Course = Course {
    -- ^ Identifier
    courseId           :: Integer

    -- ^ Relations
  , courseTutorId      :: Integer

    -- ^ Attributes
  , courseName         :: String
  , courseSemester     :: Semester
  , courseEnrollment   :: TimeSpan
  , coursePassCriteria :: Double
  } deriving (Eq, Read, Show)

data Semester = SS13 deriving (Eq, Read, Show)


-------------------------------------------------------------------------------
-- GROUP
-------------------------------------------------------------------------------

data Group = Group {
    -- ^ Identifier
    groupId       :: Integer

    -- ^ Relations
  , groupCourseId :: Integer

    -- ^ Attributes
  , groupName     :: String
  , groupCapacity :: Int
  } deriving (Eq, Read, Show)


-------------------------------------------------------------------------------
-- ENROLLMENT
-------------------------------------------------------------------------------

data Enrollment = Enrollment {
    -- ^ Identifier
    enrollmentId        :: Integer
    
    -- ^ Relations
  , enrollmentGroupId   :: Integer
  , enrollmentStudentId :: Integer

    -- ^ Attributes
  , enrollmentTime      :: UTCTime
  } deriving (Eq, Read, Show)


-------------------------------------------------------------------------------
-- STUDENT
-------------------------------------------------------------------------------

data Student = Student {
    -- ^ Identifier
    studentId            :: Integer

    -- ^ Attributes
  , studentName          :: FullName
  , studentEmail         :: String
  , studentPassword      :: String
  } deriving (Eq, Read, Show)


-------------------------------------------------------------------------------
-- TASK
-------------------------------------------------------------------------------

data Task = Task {
    -- ^ Identifier
    taskId           :: Integer

    -- ^ Relations
  , taskTutorId      :: Integer

    -- ^ Attributes
  , taskName         :: String
  , taskSignature    :: ServerHash
  , taskScoringOrder :: ScoringOrder
  } deriving (Eq, Read, Show)


-------------------------------------------------------------------------------
-- ASSIGNMENT
-------------------------------------------------------------------------------

data Assignment = Assignment {
    -- ^ Identifier
    assignmentId            :: Integer

    -- ^ Relations
  , assignmentTutorId       :: Integer
  , assignmentCourseId      :: Integer

    -- ^ Attributes
  , assignmentStatus        :: Status
  , assignmentSubmission    :: TimeSpan
  } deriving (Eq, Read, Show)

data Status = Mandatory | Optional deriving (Eq, Read, Show)


-------------------------------------------------------------------------------
-- TASK INSTANCE
-------------------------------------------------------------------------------

data TaskInstance = TaskInstance {
    -- ^ Identifier
    taskInstanceId         :: Integer

    -- ^ Relations
  , taskInstanceTaskId     :: Integer
  , taskInstanceStudentId  :: Integer

    -- ^ Attributes
  , taskInstanceDescription   :: String
  , taskInstanceDocumentation :: String
  , taskInstanceSolution      :: String
  , taskInstanceSignature     :: ServerHash
  } deriving (Eq, Read, Show)


-------------------------------------------------------------------------------
-- SOLUTION
-------------------------------------------------------------------------------

data Solution = Solution {
    -- ^ Identifier
    solutionId             :: Integer

    -- ^ Relations
  , solutionTaskInstanceId :: Integer

    -- ^ Attributes
  , solutionContent      :: String
  , solutionEvaluation   :: String
  , solutionResult       :: Maybe Result
  , solutionSubission    :: UTCTime
  } deriving (Eq, Read, Show)

data Result = Result {
    score :: Int
  , size :: Int
  } deriving (Eq, Read, Show)
