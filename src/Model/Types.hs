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

  , MongoIO
  , new
  , retrieve

  ) where

------------------------------------------------------------------------------
import           Control.Applicative ((<$>), (<*>))
import           Data.Data
import qualified Data.Text as T
import           Data.Time (UTCTime)
import           Database.MongoDB hiding (Group, group, lookup)
import qualified Database.MongoDB as Mongo
------------------------------------------------------------------------------
-- import           Autotool.Client.Types.ScoringOrder
import           Model.Indexable


------------------------------------------------------------------------------
-- | Class to convert MongoDB records into Haskell datatypes.
class MongoIO a where
  new :: a -> [Field]
  retrieve :: [Field] -> (Maybe a)


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

type AssignmentId = String

instance MongoIO Assignment where
  new t = [
      "course" =: assignmentCourse t
    , "task"   =: assignmentTask t
    , "status" =: assignmentStatus t
    , "start"  =: assignmentStart t
    , "end"    =: assignmentEnd t
    ]
  retrieve d = Assignment
    <$> Mongo.lookup "_id" d
    <*> Mongo.lookup "course" d
    <*> Mongo.lookup "task" d
    <*> Mongo.lookup "status" d
    <*> Mongo.lookup "start" d
    <*> Mongo.lookup "end" d

data Status = Mandatory
            | Optional
            deriving (Read, Show, Eq, Typeable)

instance Val Status where
    val = String . T.pack . show
    cast' (String x) = Just (read $ T.unpack x)
    cast' _ = Nothing

instance Indexable Assignment where
  iid = assignmentId
  setId assn idVal = assn { assignmentId = idVal }

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

type CourseId = String

instance Indexable Course where
  iid = courseId
  setId assn idVal = assn { courseId = idVal }

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
    <$> Mongo.lookup "_id" d
    <*> Mongo.lookup "tutor" d
    <*> Mongo.lookup "name" d
    <*> Mongo.lookup "semester" d
    <*> Mongo.lookup "enrollFrom" d
    <*> Mongo.lookup "enrollTo" d
    <*> Mongo.lookup "passCriteria" d

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

type EnrollmentId = String

instance MongoIO Enrollment where
  new t = [
      "courseGroup" =: enrollmentGroup t
    , "student"     =: enrollmentStudent t
    , "time"        =: enrollmentTime t
    ]
  retrieve d = Enrollment
    <$> Mongo.lookup "_id" d
    <*> Mongo.lookup "courseGroup" d
    <*> Mongo.lookup "student" d
    <*> Mongo.lookup "time" d

instance Indexable Enrollment where
  iid = enrollmentId
  setId assn idVal = assn { enrollmentId = idVal }

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

type GroupId = String

instance MongoIO Group where
  new g = [
      "course"      =: groupCourse g
    , "description" =: groupDescription g
    , "capacity"    =: groupCapacity g
    ]
  retrieve d = Group
    <$> Mongo.lookup "_id" d
    <*> Mongo.lookup "course" d
    <*> Mongo.lookup "description" d
    <*> Mongo.lookup "capacity" d

instance Indexable Group where
  iid = groupId
  setId group idVal = group { groupId = idVal }

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

type SolutionId = String

instance MongoIO Solution where
  new s = [
      "taskInstance" =: solutionTaskInstance s
    , "content"      =: solutionContent s
    , "evaluation"   =: solutionEvaluation s
    , "result"       =: solutionResult s
    , "submission"   =: solutionSubmission s
    ]
  retrieve d = Solution
    <$> Mongo.lookup "_id" d
    <*> Mongo.lookup "taskInstance" d
    <*> Mongo.lookup "content" d
    <*> Mongo.lookup "evaluation" d
    <*> Mongo.lookup "result" d
    <*> Mongo.lookup "submission" d

data Result = Result {
      score :: Int
    , size  :: Int
    } deriving (Eq, Read, Show, Typeable)

instance Val Result where
    val = String . T.pack . show
    cast' (String x) = Just (read $ T.unpack x)
    cast' _ = Nothing

instance Indexable Solution where
  iid = solutionId
  setId solutionconfig idVal = solutionconfig { solutionId = idVal }

------------------------------------------------------------------------------
-- | User data types have not been implemented yet.
data Student = Student {
    studentId    :: StudentId
  , studentEmail :: String
  } deriving (Eq, Read, Show)

type StudentId = String

instance MongoIO Student where
  new t = [
    "email" =: studentEmail t]
  retrieve d = Student
    <$> Mongo.lookup "_id" d
    <*> Mongo.lookup "email" d

instance Indexable Student where
  iid = studentId
  setId assn idVal = assn { studentId = idVal }

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

type TaskId = String

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
    <$> Mongo.lookup "_id" d
    <*> Mongo.lookup "tutor" d
    <*> Mongo.lookup "name" d
    <*> Mongo.lookup "type" d
    <*> Mongo.lookup "signature" d
    <*> Mongo.lookup "scoringOrder" d
    <*> Mongo.lookup "created" d

data ScoringOrder = Decreasing | Increasing | None
    deriving (Eq, Read, Show, Typeable)

instance Val ScoringOrder where
    val = String . T.pack . show
    cast' (String x) = Just (read $ T.unpack x)
    cast' _ = Nothing

instance Indexable Task where
  iid = taskId
  setId taskconfig idVal = taskconfig { taskId = idVal }

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

type TaskInstanceId = String

instance MongoIO TaskInstance where
  new t = [
      "assignment"    =: taskInstanceAssignment t
    , "student"       =: taskInstanceStudent t
    , "description"   =: taskInstanceDescription t
    , "documentation" =: taskInstanceDocumentation t
    , "solution"      =: taskInstanceSolution t
    , "signature"     =: taskInstanceSignature t
    ]
  retrieve t = TaskInstance
    <$> Mongo.lookup "_id" t
    <*> Mongo.lookup "assignment" t
    <*> Mongo.lookup "student" t
    <*> Mongo.lookup "description" t
    <*> Mongo.lookup "documentation" t
    <*> Mongo.lookup "solution" t
    <*> Mongo.lookup "signature" t

instance Indexable TaskInstance where
  iid = taskInstanceId
  setId assn idVal = assn { taskInstanceId = idVal }

------------------------------------------------------------------------------
-- | User data types have not been implemented yet.
data Tutor = Tutor {
    tutorId    :: TutorId
  , tutorEmail :: String
  } deriving (Eq, Read, Show)

type TutorId = String

instance MongoIO Tutor where
  new t = [
    "email" =: tutorEmail t]
  retrieve d = Tutor
    <$> Mongo.lookup "_id" d
    <*> Mongo.lookup "email" d

instance Indexable Tutor where
  iid = tutorId
  setId assn idVal = assn { tutorId = idVal }


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
