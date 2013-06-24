{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Model.Extensions.MongoIO (
    MongoIO
  , new
  , retrieve
  ) where

------------------------------------------------------------------------------
import           Control.Applicative ((<$>), (<*>))
import qualified Data.Text           as T
------------------------------------------------------------------------------
import           Database.MongoDB    (Field, Val, ObjectId, (=:), Value(..))
import qualified Database.MongoDB    as Mongo
------------------------------------------------------------------------------
import           Model.Datatypes

------------------------------------------------------------------------------
-- | Class to convert MongoDB records into Haskell datatypes.
class MongoIO a where
  new :: a -> [Field]
  retrieve :: [Field] -> (Maybe a)

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

