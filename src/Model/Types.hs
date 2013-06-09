module Model.Types where

import Data.Time (UTCTime)
import Autotool.Client.Types.ScoringOrder
import Model.Indexable

------------------------------------------------------------------------------
-- |
data Assignment = Assignment {
    -- ^ Identifier
    assignmentId            :: AssignmentId
    -- ^ Relations
  , assignmentCourseId      :: CourseId
  , assignmentTaskId        :: TaskId
    -- ^ Attributes
  , assignmentStatus        :: Status
  , assignmentStart         :: UTCTime
  , assignmentEnd           :: UTCTime
  } deriving (Eq, Read, Show)

type AssignmentId = Integer

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
    courseId             :: CourseId
    -- ^ Relations
  , courseTutorId        :: TutorId
  , courseGroups         :: [GroupId]
  , courseAssignments    :: [AssignmentId]
    -- ^ Attributes
  , courseName           :: String
  , courseSemester       :: String
  , courseEnrollmentFrom :: Maybe UTCTime
  , courseEnrollmentTo   :: Maybe UTCTime
  , coursePassCriteria   :: Double
  } deriving (Eq, Ord, Read, Show)

type CourseId = Integer

instance Indexable Course where
  iid = courseId
  setId course idVal = course { courseId = idVal }


------------------------------------------------------------------------------
-- |
data Enrollment = Enrollment {
    -- ^ Identifier
    enrollmentId        :: EnrollmentId
    -- ^ Relations
  , enrollmentGroupId   :: GroupId
  , enrollmentStudentId :: StudentId
    -- ^ Attributes
  , enrollmentTime      :: UTCTime
  } deriving (Eq, Read, Show)

type EnrollmentId = Integer

instance Indexable Enrollment where
  iid = enrollmentId
  setId assn idVal = assn { enrollmentId = idVal }


------------------------------------------------------------------------------
-- |
data Group = Group {
    -- ^ Identifier
    groupId             :: GroupId
    -- ^ Relations
  , groupCourseId       :: CourseId
  , groupEnrollments    :: [EnrollmentId]
    -- ^ Attributes
  , groupDescription    :: String
  , groupCapacity       :: Int
  } deriving (Eq, Ord, Read, Show)

type GroupId = Integer

instance Indexable Group where
  iid = groupId
  setId group idVal = group { groupId = idVal }


------------------------------------------------------------------------------
-- |
data Solution = Solution {
    -- ^ Identifier
      solutionId             :: SolutionId
    -- ^ Relations
    , solutionTaskInstanceId :: TaskInstanceId
    -- ^ Attributes
    , solutionContent        :: String
    , solutionEvaluation     :: String
    , solutionResult         :: Maybe Result
    , solutionSubmission     :: UTCTime
    } deriving (Read, Show)

type SolutionId = Integer

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
      taskId            :: TaskId
    -- ^ Relations
    , taskTutorId       :: TutorId
    , taskAssignments   :: [AssignmentId]
    , taskTaskInstances :: [TaskInstanceId]
    -- ^ Attributes
    , taskName          :: String
    , taskType          :: String
    , taskSignature     :: String
    , taskScoringOrder  :: ScoringOrder
    , taskCreated       :: UTCTime
    } deriving (Read, Show)

type TaskId = Integer

instance Indexable Task where
  iid = taskId
  setId taskconfig idVal = taskconfig { taskId = idVal }


------------------------------------------------------------------------------
-- |
data TaskInstance = TaskInstance {
    -- ^ Identifier
    taskInstanceId            :: TaskInstanceId
    -- ^ Relations
  , taskInstanceTaskId        :: TaskId
  , taskInstanceStudentId     :: StudentId
  , taskInstanceSolutions     :: [SolutionId]
    -- ^ Attributes
  , taskInstanceDescription   :: String
  , taskInstanceDocumentation :: String
  , taskInstanceSolution      :: String
  , taskInstanceSignature     :: String
  } deriving (Eq, Read, Show)

type TaskInstanceId = Integer

instance Indexable TaskInstance where
  iid = taskInstanceId
  setId assn idVal = assn { taskInstanceId = idVal }


------------------------------------------------------------------------------
-- | User data types have not been implemented yet.
data Tutor = Tutor {
    tutorId      :: TutorId
  , tutorCourses :: [CourseId]
  , tutorTasks   :: [TaskId]
  } deriving (Eq, Read, Show)

type TutorId   = Integer

instance Indexable Tutor where
  iid = tutorId
  setId assn idVal = assn { tutorId = idVal }


------------------------------------------------------------------------------
-- | User data types have not been implemented yet.
data Student = Student {
    studentId            :: StudentId
  , studentEnrollments   :: [EnrollmentId]
  , studentTaskInstances :: [TaskInstanceId]
  } deriving (Eq, Read, Show)

type StudentId = Integer

instance Indexable Student where
  iid = studentId
  setId assn idVal = assn { studentId = idVal }
