{-# LANGUAGE OverloadedStrings #-}
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

  , Indexable
  , iid
  , setId

  , MongoIO
  , new
  , retrieve

  ) where

------------------------------------------------------------------------------
import           Control.Applicative ((<$>), (<*>))
import           Data.Data
import qualified Data.Text as T
import           Data.Time (UTCTime)
import           Database.MongoDB hiding (Field, Group, group, lookup)
import qualified Database.MongoDB as Mongo
import           Database.SQLite.Simple
import           Database.SQLite.Simple.FromField
import           Database.SQLite.Simple.ToField
import           Database.SQLite.Simple.Internal
import           Database.SQLite.Simple.Ok


------------------------------------------------------------------------------ 
-- | Id Types.
type AssignmentId   = String
type CourseId       = String
type EnrollmentId   = String
type GroupId        = String
type SolutionId     = String
type StudentId      = String
type TaskId         = String
type TaskInstanceId = String
type TutorId        = String


------------------------------------------------------------------------------
-- |
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
  } deriving (Eq, Read, Show)

data Status = Mandatory
            | Optional
            deriving (Read, Show, Eq, Typeable)

------------------------------------------------------------------------------
-- |
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
  } deriving (Eq, Read, Show)

------------------------------------------------------------------------------
-- |
data Enrollment = Enrollment {
    -- ^ Identifier
    enrollmentId      :: EnrollmentId
    -- ^ Relations
  , enrollmentGroup   :: GroupId
  , enrollmentStudent :: StudentId
    -- ^ Attributes
  , enrollmentTime    :: UTCTime
  } deriving (Eq, Read, Show)

------------------------------------------------------------------------------
-- |
data Group = Group {
    -- ^ Identifier
    groupId          :: GroupId
    -- ^ Relations
  , groupCourse      :: CourseId
    -- ^ Attributes
  , groupDescription :: String
  , groupCapacity    :: Int
  } deriving (Eq, Ord, Read, Show)

------------------------------------------------------------------------------
-- |
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
    } deriving (Read, Show)

data Result = Result {
      score :: Int
    , size  :: Int
    } deriving (Eq, Read, Show, Typeable)

------------------------------------------------------------------------------
-- |
data Student = Student {
    studentId    :: StudentId
  , studentEmail :: String
  } deriving (Eq, Read, Show)

------------------------------------------------------------------------------
-- |
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
    } deriving (Read, Show)

data ScoringOrder = Decreasing | Increasing | None
    deriving (Eq, Read, Show, Typeable)

------------------------------------------------------------------------------
-- |
data TaskInstance = TaskInstance {
    -- ^ Identifier
    taskInstanceId            :: TaskInstanceId
    -- ^ Relations
  , taskInstanceAssignment    :: AssignmentId
  , taskInstanceStudent       :: StudentId
    -- ^ Attributes
  , taskInstanceDescription   :: String
  , taskInstanceDocumentation :: String
  , taskInstanceSolution      :: String
  , taskInstanceSignature     :: String
  } deriving (Eq, Read, Show)

------------------------------------------------------------------------------
-- | User data types have not been implemented yet.
data Tutor = Tutor {
    tutorId    :: TutorId
  , tutorEmail :: String
  } deriving (Eq, Read, Show)


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

type TaskValues =
       (TutorId, String, String, String, ScoringOrder, UTCTime)

type TaskInstanceValues =
       (AssignmentId, StudentId, String, String, String, String)


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


------------------------------------------------------------------------------ 
-- | Instances for SQLite

instance FromRow Assignment where
  fromRow = Assignment
              <$> (fmap show (field :: RowParser Int))
              <*> (fmap show (field :: RowParser Int))
              <*> (fmap show (field :: RowParser Int))
              <*> field
              <*> field
              <*> field

instance FromRow Course where
  fromRow = Course
              <$> (fmap show (field :: RowParser Int))
              <*> field
              <*> field
              <*> field
              <*> field
              <*> field
              <*> field

instance FromRow Enrollment where
  fromRow = Enrollment
              <$> (fmap show (field :: RowParser Int))
              <*> (fmap show (field :: RowParser Int))
              <*> field
              <*> field

instance FromRow Group where
  fromRow = Group
              <$> (fmap show (field :: RowParser Int))
              <*> (fmap show (field :: RowParser Int))
              <*> field
              <*> field

instance FromRow Solution where
  fromRow = Solution
              <$> (fmap show (field :: RowParser Int))
              <*> (fmap show (field :: RowParser Int))
              <*> field
              <*> field
              <*> field
              <*> field

instance FromRow Student where
  fromRow = Student
              <$> field
              <*> field

instance FromRow Task where
  fromRow = Task
              <$> (fmap show (field :: RowParser Int))
              <*> field
              <*> field
              <*> field
              <*> field
              <*> field
              <*> field

instance FromRow TaskInstance where
  fromRow = TaskInstance
              <$> (fmap show (field :: RowParser Int))
              <*> (fmap show (field :: RowParser Int))
              <*> field
              <*> field
              <*> field
              <*> field
              <*> field

instance FromRow Tutor where
  fromRow = Tutor
              <$> field
              <*> field

instance FromField Result where
  fromField (Field (SQLText text) _) = Ok . read . T.unpack $ text
  fromField f = returnError ConversionFailed f "expected text"

instance FromField Status where
  fromField (Field (SQLText text) _) = Ok . read . T.unpack $ text
  fromField f = returnError ConversionFailed f "expected text"

instance FromField ScoringOrder where
  fromField (Field (SQLText text) _) = Ok . read . T.unpack $ text
  fromField f = returnError ConversionFailed f "expected text"

instance ToField Result where
  toField = SQLText . T.pack . show

instance ToField Status where
  toField = SQLText . T.pack . show

instance ToField ScoringOrder where
  toField = SQLText . T.pack . show


------------------------------------------------------------------------------
-- | Class to convert MongoDB records into Haskell datatypes.
class MongoIO a where
  new :: a -> [Mongo.Field]
  retrieve :: [Mongo.Field] -> (Maybe a)

------------------------------------------------------------------------------
-- | MongoDB conversion instances.
instance MongoIO Assignment where
  new t = [
      "course" =: assignmentCourse t
    , "task"   =: assignmentTask t
    , "status" =: assignmentStatus t
    , "start"  =: assignmentStart t
    , "end"    =: assignmentEnd t
    ]
  retrieve d = Assignment
    <$> (show <$> ((Mongo.lookup "_id" d) :: Maybe ObjectId))
    <*> Mongo.lookup "course" d
    <*> Mongo.lookup "task" d
    <*> Mongo.lookup "status" d
    <*> Mongo.lookup "start" d
    <*> Mongo.lookup "end" d

instance MongoIO Course where
  new t = [
      "tutor"        =: courseTutor t
    , "name"         =: courseName t
    , "semester"     =: courseSemester t
    , "enrollFrom"   =: courseEnrollmentFrom t
    , "enrollTo"     =: courseEnrollmentTo t
    , "passCriteria" =: coursePassCriteria t
    ]
  retrieve d = Course
    <$> (show <$> ((Mongo.lookup "_id" d) :: Maybe ObjectId))
    <*> Mongo.lookup "tutor" d
    <*> Mongo.lookup "name" d
    <*> Mongo.lookup "semester" d
    <*> Mongo.lookup "enrollFrom" d
    <*> Mongo.lookup "enrollTo" d
    <*> Mongo.lookup "passCriteria" d

instance MongoIO Enrollment where
  new t = [
      "courseGroup" =: enrollmentGroup t
    , "student"     =: enrollmentStudent t
    , "time"        =: enrollmentTime t
    ]
  retrieve d = Enrollment
    <$> (show <$> ((Mongo.lookup "_id" d) :: Maybe ObjectId))
    <*> Mongo.lookup "courseGroup" d
    <*> Mongo.lookup "student" d
    <*> Mongo.lookup "time" d

instance MongoIO Group where
  new g = [
      "course"      =: groupCourse g
    , "description" =: groupDescription g
    , "capacity"    =: groupCapacity g
    ]
  retrieve d = Group
    <$> (show <$> ((Mongo.lookup "_id" d) :: Maybe ObjectId))
    <*> Mongo.lookup "course" d
    <*> Mongo.lookup "description" d
    <*> Mongo.lookup "capacity" d

instance MongoIO Solution where
  new s = [
      "taskInstance" =: solutionTaskInstance s
    , "content"      =: solutionContent s
    , "evaluation"   =: solutionEvaluation s
    , "result"       =: solutionResult s
    , "submission"   =: solutionSubmission s
    ]
  retrieve d = Solution
    <$> (show <$> ((Mongo.lookup "_id" d) :: Maybe ObjectId))
    <*> Mongo.lookup "taskInstance" d
    <*> Mongo.lookup "content" d
    <*> Mongo.lookup "evaluation" d
    <*> Mongo.lookup "result" d
    <*> Mongo.lookup "submission" d

instance MongoIO Student where
  new t = [
    "email" =: studentEmail t]
  retrieve d = Student
    <$> (show <$> ((Mongo.lookup "_id" d) :: Maybe ObjectId))
    <*> Mongo.lookup "email" d

instance MongoIO Task where
  new t = [
      "tutor"        =: taskTutor t
    , "name"         =: taskName t
    , "type"         =: taskType t
    , "signature"    =: taskSignature t
    , "scoringOrder" =: taskScoringOrder t
    , "created"      =: taskCreated t
    ]
  retrieve d = Task
    <$> (show <$> ((Mongo.lookup "_id" d) :: Maybe ObjectId))
    <*> Mongo.lookup "tutor" d
    <*> Mongo.lookup "name" d
    <*> Mongo.lookup "type" d
    <*> Mongo.lookup "signature" d
    <*> Mongo.lookup "scoringOrder" d
    <*> Mongo.lookup "created" d

instance MongoIO TaskInstance where
  new t = [
      "assignment"    =: taskInstanceAssignment t
    , "student"       =: taskInstanceStudent t
    , "description"   =: taskInstanceDescription t
    , "documentation" =: taskInstanceDocumentation t
    , "solution"      =: taskInstanceSolution t
    , "signature"     =: taskInstanceSignature t
    ]
  retrieve d = TaskInstance
    <$> (show <$> ((Mongo.lookup "_id" d) :: Maybe ObjectId))
    <*> Mongo.lookup "assignment" d
    <*> Mongo.lookup "student" d
    <*> Mongo.lookup "description" d
    <*> Mongo.lookup "documentation" d
    <*> Mongo.lookup "solution" d
    <*> Mongo.lookup "signature" d

instance MongoIO Tutor where
  new t = [
    "email" =: tutorEmail t]
  retrieve d = Tutor
    <$> (show <$> ((Mongo.lookup "_id" d) :: Maybe ObjectId))
    <*> Mongo.lookup "email" d

instance Val ScoringOrder where
    val = String . T.pack . show
    cast' (String x) = Just (read $ T.unpack x)
    cast' _ = Nothing

instance Val Status where
    val = String . T.pack . show
    cast' (String x) = Just (read $ T.unpack x)
    cast' _ = Nothing

instance Val Result where
    val = String . T.pack . show
    cast' (String x) = Just (read $ T.unpack x)
    cast' _ = Nothing

