{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Model.Types where

------------------------------------------------------------------------------
import           Control.Applicative ((<$>), (<*>))
import           Data.Data
import qualified Data.Text as T
import           Data.Time (UTCTime)
import           Database.SQLite.Simple
import           Database.SQLite.Simple.FromField
import           Database.SQLite.Simple.ToField
import           Database.SQLite.Simple.Internal
import           Database.SQLite.Simple.Ok
------------------------------------------------------------------------------
-- import           Autotool.Client.Types.ScoringOrder
import           Model.Indexable


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
            deriving (Read, Show, Eq, Typeable)

instance FromField Status where
  fromField (Field (SQLText text) _) = Ok . read . T.unpack $ text
  fromField f = returnError ConversionFailed f "expected text"

instance ToField Status where
  toField = SQLText . T.pack . show

instance Indexable Assignment where
  iid = assignmentId
  setId assn idVal = assn { assignmentId = idVal }

instance FromRow Assignment where
  fromRow = Assignment <$> field <*> field <*> field <*> field <*> field
                       <*> field

------------------------------------------------------------------------------
-- |
data Course = Course {
    -- ^ Identifier
    courseId             :: CourseId
    -- ^ Relations
  , courseTutorId        :: TutorId
    -- ^ Attributes
  , courseName           :: String
  , courseSemester       :: String
  , courseEnrollmentFrom :: Maybe UTCTime
  , courseEnrollmentTo   :: Maybe UTCTime
  , coursePassCriteria   :: Double
  } deriving (Eq, Read, Show)

type CourseId = Integer

instance Indexable Course where
  iid = courseId
  setId course idVal = course { courseId = idVal }

instance FromRow Course where
  fromRow = Course <$> field <*> field <*> field <*> field <*> field <*> field
                   <*> field

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

instance FromRow Enrollment where
  fromRow = Enrollment <$> field <*> field <*> field <*> field


------------------------------------------------------------------------------
-- |
data Group = Group {
    -- ^ Identifier
    groupId             :: GroupId
    -- ^ Relations
  , groupCourseId       :: CourseId
    -- ^ Attributes
  , groupDescription    :: String
  , groupCapacity       :: Int
  } deriving (Eq, Ord, Read, Show)

type GroupId = Integer

instance Indexable Group where
  iid = groupId
  setId group idVal = group { groupId = idVal }

instance FromRow Group where
  fromRow = Group <$> field <*> field <*> field <*> field

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
    } deriving (Eq, Read, Show, Typeable)

instance FromField Result where
  fromField (Field (SQLText text) _) = Ok . read . T.unpack $ text
  fromField f = returnError ConversionFailed f "expected text"

instance ToField Result where
  toField = SQLText . T.pack . show

instance Indexable Solution where
  iid = solutionId
  setId solutionconfig idVal = solutionconfig { solutionId = idVal }

instance FromRow Solution where
  fromRow = Solution <$> field <*> field <*> field <*> field <*> field
                     <*> field

------------------------------------------------------------------------------
-- | User data types have not been implemented yet.
data Student = Student {
    studentId    :: StudentId
  , studentEmail :: String
  } deriving (Eq, Read, Show)

type StudentId = Integer

instance Indexable Student where
  iid = studentId
  setId assn idVal = assn { studentId = idVal }

instance FromRow Student where
  fromRow = Student <$> field <*> field

------------------------------------------------------------------------------
-- |
data Task = Task {
    -- ^ Identifier
      taskId            :: TaskId
    -- ^ Relations
    , taskTutorId       :: TutorId
    -- ^ Attributes
    , taskName          :: String
    , taskType          :: String
    , taskSignature     :: String
    , taskScoringOrder  :: ScoringOrder
    , taskCreated       :: UTCTime
    } deriving (Read, Show)

type TaskId = Integer

data ScoringOrder = Decreasing | Increasing | None
    deriving (Eq, Read, Show, Typeable)

instance FromField ScoringOrder where
  fromField (Field (SQLText text) _) = Ok . read . T.unpack $ text
  fromField f = returnError ConversionFailed f "expected text"

instance ToField ScoringOrder where
  toField = SQLText . T.pack . show

instance Indexable Task where
  iid = taskId
  setId taskconfig idVal = taskconfig { taskId = idVal }

instance FromRow Task where
  fromRow = Task <$> field <*> field <*> field <*> field <*> field <*> field
                 <*> field

------------------------------------------------------------------------------
-- |
data TaskInstance = TaskInstance {
    -- ^ Identifier
    taskInstanceId            :: TaskInstanceId
    -- ^ Relations
  , taskInstanceAssignmentId  :: AssignmentId
  , taskInstanceStudentId     :: StudentId
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

instance FromRow TaskInstance where
  fromRow = TaskInstance <$> field <*> field <*> field <*> field <*> field
                         <*> field <*> field


------------------------------------------------------------------------------
-- | User data types have not been implemented yet.
data Tutor = Tutor {
    tutorId    :: TutorId
  , tutorEmail :: String
  } deriving (Eq, Read, Show)

type TutorId = Integer

instance Indexable Tutor where
  iid = tutorId
  setId assn idVal = assn { tutorId = idVal }

instance FromRow Tutor where
  fromRow = Tutor <$> field <*> field


------------------------------------------------------------------------------ 
-- | Value Types.

type AssignmentValues =
       (CourseId, TaskId, Status, UTCTime, UTCTime)

type CourseValues =
       (TutorId, String, String, Maybe UTCTime, Maybe UTCTime, Double)

type EnrollmentValues =
       (GroupId, StudentId, UTCTime)

type GroupValues =
       (CourseId, String, Int)

type SolutionValues =
       (TaskInstanceId, String, String, Maybe Result, UTCTime)

type StudentValues =
       (String)

type TaskValues =
       (TutorId, String, String, String, ScoringOrder, UTCTime)

type TaskInstanceValues =
       (AssignmentId, StudentId, String, String, String, String)

type TutorValues =
       (String)

