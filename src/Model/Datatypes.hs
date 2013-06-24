{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Model.Datatypes (
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
import           Data.Data
import           Data.Time (UTCTime)


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
-- | Value Types use for construction, since the id is mostly created at db
-- level.

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
