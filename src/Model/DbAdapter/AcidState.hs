{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE OverloadedStrings          #-}

module Model.DbAdapter.AcidState (
    -- ^ retrieve all
    getAllCourses

    -- ^ retrieve one by primary key
  , getCourse
  , getStudent
  , getTask
  , getTaskInstance
  , getTutor

    -- ^ retrieve many by primary jey 
  , getGroups

    -- ^ retrieve many by foreign key
  , getAssignmentsByCourse
  , getAssignmentsByTask
  , getCoursesByTutor
  , getEnrollmentsByStudent
  , getGroupsByCourse
  , getTasksByTutor
  , getTaskInstancesByAssignment
  , getSolutionsByTaskInstance

    -- ^ create
  , createAssignment
  , createCourse
  , createEnrollment
  , createGroup
  , createSolution
  , createTask
  , createTaskInstance

    -- ^ other
  , getLastSolutionsByTaskInstance

    -- ^ helper

  ) where

------------------------------------------------------------------------------
import           Control.Monad.Reader      (asks)
import           Data.Map                  (Map)
-- import           Control.Monad             (unless)
-- import           Data.Text                 (Text)
-- import qualified Data.Text                 as T
------------------------------------------------------------------------------
import           Snap                      ((<$>), liftIO)
import           Snap.Snaplet.AcidState    (Query, makeAcidic, query)
------------------------------------------------------------------------------
import           Application
import           Model.Types


askAssignments :: Query AssignmentStore (Map Integer Assignment)
askAssignments = asks _assignmentStore

makeAcidic ''AssignmentStore ['askAssignments]

------------------------------------------------------------------------------ 
-- | Retrieve.

getAssignmentsByCourse :: CourseId -> AppHandler [Assignment]
getAssignmentsByCourse cid =
    query AskAssignments
    undefined


getAssignmentsByTask :: TaskId -> AppHandler [Assignment]
getAssignmentsByTask tid =
    undefined

------------------------------------------------------------------------------ 

getCourse :: CourseId -> AppHandler (Maybe Course)
getCourse cid =
    undefined

getAllCourses :: AppHandler [Course]
getAllCourses =
    undefined

getCoursesByTutor :: TutorId -> AppHandler [Course]
getCoursesByTutor tid =
  undefined

------------------------------------------------------------------------------ 

getEnrollmentsByStudent :: StudentId -> AppHandler [Enrollment]
getEnrollmentsByStudent sid =
  undefined

------------------------------------------------------------------------------ 

getGroups :: [GroupId] -> AppHandler [Group]
getGroups gids = do
    undefined

getGroupsByCourse :: CourseId -> AppHandler [Group]
getGroupsByCourse tid =
    undefined

------------------------------------------------------------------------------ 

getSolutionsByTaskInstance :: TaskInstanceId -> AppHandler [Solution]
getSolutionsByTaskInstance tid =
    undefined

getLastSolutionsByTaskInstance :: TaskInstanceId -> AppHandler (Maybe Solution)
getLastSolutionsByTaskInstance tid =
    undefined

------------------------------------------------------------------------------ 

getStudent :: StudentId -> AppHandler (Maybe Student)
getStudent sid =
    undefined

------------------------------------------------------------------------------ 

getTask :: TaskId -> AppHandler (Maybe Task)
getTask tid =
    undefined

getTasksByTutor :: TutorId -> AppHandler [Task]
getTasksByTutor tid =
    undefined

------------------------------------------------------------------------------ 

getTaskInstance :: TaskInstanceId -> AppHandler (Maybe TaskInstance)
getTaskInstance tid =
    undefined

getTaskInstancesByAssignment :: AssignmentId -> AppHandler [TaskInstance]
getTaskInstancesByAssignment aid =
    undefined

------------------------------------------------------------------------------ 

getTutor :: Integer -> AppHandler (Maybe Tutor)
getTutor tid =
    undefined



------------------------------------------------------------------------------ 
-- | Generic create function.
-- create  :: (S.ToRow a) => String -> a -> AppHandler Integer
-- create insertQuery values =
    -- undefined


------------------------------------------------------------------------------ 
-- | Specific setters.

createAssignment :: AssignmentValues -> AppHandler AssignmentId
createAssignment =
    undefined
     
createCourse :: CourseValues -> AppHandler CourseId
createCourse =
    undefined

createEnrollment :: EnrollmentValues -> AppHandler EnrollmentId
createEnrollment =
    undefined

createGroup :: GroupValues -> AppHandler GroupId
createGroup =
    undefined

createSolution :: SolutionValues -> AppHandler SolutionId
createSolution =
    undefined

createTask :: TaskValues -> AppHandler TaskId
createTask =
    undefined

createTaskInstance :: TaskInstanceValues -> AppHandler TaskInstanceId
createTaskInstance =
    undefined


------------------------------------------------------------------------------ 
-- | Utils.

listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe xs = Just $ head xs

