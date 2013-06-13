{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Model.Types (
    -- ^ main types
    Assignment   (..)
  , Course       (..)
  , Enrollment   (..)
  , Group        (..)
  , Solution     (..)
  , Student      (..)
  , Task         (..)
  , TaskInstance (..)
  , Tutor        (..)

    -- ^ id types
  , AssignmentId
  , CourseId
  , EnrollmentId
  , GroupId
  , SolutionId
  , StudentId
  , TaskId
  , TaskInstanceId
  , TutorId

    -- ^ additional types
  , Result       (..)
  , ScoringOrder (..)
  , Status       (..)

    -- ^ value tuples
  , AssignmentValues
  , CourseValues
  , EnrollmentValues
  , GroupValues
  , SolutionValues
  , TaskValues
  , TaskInstanceValues

  ) where

------------------------------------------------------------------------------
import           Control.Applicative ((<$>), (<*>))
import           Data.Data
import           Data.SafeCopy             (deriveSafeCopy, base)
import qualified Data.Text as T
import           Data.Time (UTCTime)
import           Database.SQLite.Simple
import           Database.SQLite.Simple.FromField
import           Database.SQLite.Simple.ToField
import           Database.SQLite.Simple.Internal
import           Database.SQLite.Simple.Ok
------------------------------------------------------------------------------
import           Model.Indexable


------------------------------------------------------------------------------
-- | Id types
------------------------------------------------------------------------------

type AssignmentId   = Integer
type CourseId       = Integer
type EnrollmentId   = Integer
type GroupId        = Integer
type SolutionId     = Integer
type StudentId      = Integer
type TaskId         = Integer
type TaskInstanceId = Integer
type TutorId        = Integer


------------------------------------------------------------------------------
-- | User types
------------------------------------------------------------------------------

------------------------------------------------------------------------------
-- | Tutor
data Tutor = Tutor {
    tutorId    :: TutorId
  , tutorEmail :: String
  } deriving (Eq, Read, Show, Typeable)

instance Indexable Tutor where
  iid = tutorId
  setId assn idVal = assn { tutorId = idVal }

instance FromRow Tutor where
  fromRow = Tutor <$> field <*> field

deriveSafeCopy 0 'base ''Tutor

------------------------------------------------------------------------------
-- | Student
data Student = Student {
    studentId    :: StudentId
  , studentEmail :: String
  } deriving (Eq, Read, Show, Typeable)

instance Indexable Student where
  iid = studentId
  setId assn idVal = assn { studentId = idVal }

instance FromRow Student where
  fromRow = Student <$> field <*> field

deriveSafeCopy 0 'base ''Student


------------------------------------------------------------------------------
-- | Organisation Types
------------------------------------------------------------------------------

------------------------------------------------------------------------------
-- | Course
data Course = Course {
    -- ^ Identifier
    courseId             :: CourseId
    -- ^ Relations
  , courseTutor          :: TutorId
    -- ^ Attributes
  , courseName           :: String
  , courseSemester       :: String
  , courseEnrollmentFrom :: Maybe UTCTime
  , courseEnrollmentTo   :: Maybe UTCTime
  , coursePassCriteria   :: Double
  } deriving (Eq, Read, Show, Typeable)

instance Indexable Course where
  iid = courseId
  setId course idVal = course { courseId = idVal }

instance FromRow Course where
  fromRow = Course <$> field <*> field <*> field <*> field <*> field <*> field
                   <*> field

deriveSafeCopy 0 'base ''Course

------------------------------------------------------------------------------
-- | Group
data Group = Group {
    -- ^ Identifier
    groupId          :: GroupId
    -- ^ Relations
  , groupCourse      :: CourseId
    -- ^ Attributes
  , groupDescription :: String
  , groupCapacity    :: Int
  } deriving (Eq, Read, Show, Typeable)

instance Indexable Group where
  iid = groupId
  setId group idVal = group { groupId = idVal }

instance FromRow Group where
  fromRow = Group <$> field <*> field <*> field <*> field

deriveSafeCopy 0 'base ''Group

------------------------------------------------------------------------------
-- | Enrollment
data Enrollment = Enrollment {
    -- ^ Identifier
    enrollmentId      :: EnrollmentId
    -- ^ Relations
  , enrollmentGroup   :: GroupId
  , enrollmentStudent :: StudentId
    -- ^ Attributes
  , enrollmentTime    :: UTCTime
  } deriving (Eq, Read, Show, Typeable)

deriveSafeCopy 0 'base ''Enrollment

instance Indexable Enrollment where
  iid = enrollmentId
  setId assn idVal = assn { enrollmentId = idVal }

instance FromRow Enrollment where
  fromRow = Enrollment <$> field <*> field <*> field <*> field


------------------------------------------------------------------------------
-- | Operational types
------------------------------------------------------------------------------

------------------------------------------------------------------------------
-- | Task
data ScoringOrder = Decreasing | Increasing | None
    deriving (Eq, Read, Show, Typeable)

instance FromField ScoringOrder where
  fromField (Field (SQLText text) _) = Ok . read . T.unpack $ text
  fromField f = returnError ConversionFailed f "expected text"

instance ToField ScoringOrder where
  toField = SQLText . T.pack . show

deriveSafeCopy 0 'base ''ScoringOrder

data Task = Task {
    -- ^ Identifier
      taskId           :: TaskId
    -- ^ Relations
    , taskTutor        :: TutorId
    -- ^ Attributes
    , taskName         :: String
    , taskType         :: String
    , taskSignature    :: String
    , taskScoringOrder :: ScoringOrder
    , taskCreated      :: UTCTime
    } deriving (Eq, Read, Show, Typeable)

instance Indexable Task where
  iid = taskId
  setId taskconfig idVal = taskconfig { taskId = idVal }

instance FromRow Task where
  fromRow = Task <$> field <*> field <*> field <*> field <*> field <*> field
                 <*> field

deriveSafeCopy 0 'base ''Task

------------------------------------------------------------------------------
-- | Assignment
data Status =
    Mandatory
  | Optional
    deriving (Eq, Read, Show, Typeable)

instance FromField Status where
  fromField (Field (SQLText text) _) = Ok . read . T.unpack $ text
  fromField f = returnError ConversionFailed f "expected text"

instance ToField Status where
  toField = SQLText . T.pack . show

deriveSafeCopy 0 'base ''Status

data Assignment = Assignment {
    -- ^ Identifier
    assignmentId     :: AssignmentId
    -- ^ Relations
  , assignmentCourse :: CourseId
  , assignmentTask   :: TaskId
    -- ^ Attributes
  , assignmentStatus :: Status
  , assignmentStart  :: UTCTime
  , assignmentEnd    :: UTCTime
  } deriving (Eq, Read, Show, Typeable)


instance Indexable Assignment where
  iid = assignmentId
  setId assn idVal = assn { assignmentId = idVal }

instance FromRow Assignment where
  fromRow = Assignment <$> field <*> field <*> field <*> field <*> field
                       <*> field

deriveSafeCopy 0 'base ''Assignment

------------------------------------------------------------------------------
-- | TaskInstance
data TaskInstance = TaskInstance {
    -- ^ Identifier
    taskInstanceId            :: TaskInstanceId
    -- ^ Relations
  , taskInstanceAssignment    :: AssignmentId
  , taskInstanceStudentId     :: StudentId
    -- ^ Attributes
  , taskInstanceDescription   :: String
  , taskInstanceDocumentation :: String
  , taskInstanceSolution      :: String
  , taskInstanceSignature     :: String
  } deriving (Eq, Read, Show, Typeable)

instance Indexable TaskInstance where
  iid = taskInstanceId
  setId assn idVal = assn { taskInstanceId = idVal }

instance FromRow TaskInstance where
  fromRow = TaskInstance <$> field <*> field <*> field <*> field <*> field
                         <*> field <*> field

deriveSafeCopy 0 'base ''TaskInstance

------------------------------------------------------------------------------
-- | Solution
data Result = Result {
      score :: Int
    , size  :: Int
    } deriving (Eq, Read, Show, Typeable)

instance FromField Result where
  fromField (Field (SQLText text) _) = Ok . read . T.unpack $ text
  fromField f = returnError ConversionFailed f "expected text"

instance ToField Result where
  toField = SQLText . T.pack . show

deriveSafeCopy 0 'base ''Result

data Solution = Solution {
    -- ^ Identifier
      solutionId           :: SolutionId
    -- ^ Relations
    , solutionTaskInstance :: TaskInstanceId
    -- ^ Attributes
    , solutionContent      :: String
    , solutionEvaluation   :: String
    , solutionResult       :: Maybe Result
    , solutionSubmission   :: UTCTime
    } deriving (Eq, Read, Show, Typeable)

instance Indexable Solution where
  iid = solutionId
  setId solutionconfig idVal = solutionconfig { solutionId = idVal }

instance FromRow Solution where
  fromRow = Solution <$> field <*> field <*> field <*> field <*> field
                     <*> field

deriveSafeCopy 0 'base ''Solution


------------------------------------------------------------------------------ 
-- | Value Types
------------------------------------------------------------------------------ 

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

type TaskValues =
       (TutorId, String, String, String, ScoringOrder, UTCTime)

type TaskInstanceValues =
       (AssignmentId, StudentId, String, String, String, String)
