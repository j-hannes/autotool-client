{-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE ScopedTypeVariables #-}

module Model.DbAdapter.MongoDB (
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
  , getLastSolutionByTaskInstance

    -- ^ helper
  -- , createTables

  ) where

import Snap
------------------------------------------------------------------------------
import Data.Text (Text)
import           Data.Maybe (catMaybes)
import           Database.MongoDB          hiding (Group, lookup, group)
import qualified Database.MongoDB as Mongo

------------------------------------------------------------------------------
import           Snap.Snaplet.MongoDB
------------------------------------------------------------------------------
import           Application               (AppHandler)
import           Model.Types


------------------------------------------------------------------------------ 
-- | Generic getters.
getOne :: (MongoIO a) => Collection -> String -> AppHandler (Maybe a)
getOne name _id = do
    result <- eitherWithDB $ rest =<<
              find (select ["_id" =: _id] name)
    let records = either (const []) id result
    return $ if null records
                then Nothing
                else head $ map retrieve records

getMany :: (Show a, MongoIO a) => Collection -> Selector -> AppHandler [a]
getMany name condition = do
    result <- eitherWithDB $ rest =<<
              find (select condition name)
    let records = either (const []) id result
    let f = head records
    liftIO $ print f
    let g = Mongo.lookup "_id" f :: Maybe ObjectId
    liftIO $ print g
    let r = retrieve f
    liftIO $ print r
    return $ catMaybes [r]

------------------------------------------------------------------------------ 
-- | Retrieve.

getAssignmentsByCourse :: CourseId -> AppHandler [Assignment]
getAssignmentsByCourse cid = getMany "assignment" ["course" =: cid]

getAssignmentsByTask :: TaskId -> AppHandler [Assignment]
getAssignmentsByTask tid = getMany "assignment" ["task" =: tid]

------------------------------------------------------------------------------ 

getCourse :: CourseId -> AppHandler (Maybe Course)
getCourse = getOne "course"

getAllCourses :: AppHandler [Course]
getAllCourses = getMany "course" []

getCoursesByTutor :: TutorId -> AppHandler [Course]
getCoursesByTutor tid = getMany "course" ["tutor" := val tid]

------------------------------------------------------------------------------ 

getEnrollmentsByStudent :: StudentId -> AppHandler [Enrollment]
getEnrollmentsByStudent sid = getMany "enrollment" ["student" := val sid]

------------------------------------------------------------------------------ 

getGroups :: [GroupId] -> AppHandler [Group]
getGroups gids = do
    groups <- getMany "courseGroup" []
    return $ filter (\g -> groupId g `elem` gids) groups

getGroupsByCourse :: CourseId -> AppHandler [Group]
getGroupsByCourse cid = getMany "courseGroup" ["course" := val cid]

------------------------------------------------------------------------------ 

getSolutionsByTaskInstance :: TaskInstanceId -> AppHandler [Solution]
getSolutionsByTaskInstance tid = getMany "solution" ["taskInstance" := val tid]

getLastSolutionByTaskInstance :: TaskInstanceId -> AppHandler (Maybe Solution)
getLastSolutionByTaskInstance tid = do
    solutions <- getSolutionsByTaskInstance tid
    if null solutions
      then return Nothing
      else return . Just $ last solutions

------------------------------------------------------------------------------ 

getStudent :: StudentId -> AppHandler (Maybe Student)
getStudent = getOne "student"

------------------------------------------------------------------------------ 

getTask :: TaskId -> AppHandler (Maybe Task)
getTask = getOne "task"

getTasksByTutor :: TutorId -> AppHandler [Task]
getTasksByTutor tid = getMany "task" ["tutor" := val tid]

------------------------------------------------------------------------------ 

getTaskInstance :: TaskInstanceId -> AppHandler (Maybe TaskInstance)
getTaskInstance = getOne "taskInstance"

getTaskInstancesByAssignment :: AssignmentId -> AppHandler [TaskInstance]
getTaskInstancesByAssignment aid = getMany "taskInstance"
                                           ["assignment" := val aid]

------------------------------------------------------------------------------ 

getTutor :: TutorId -> AppHandler (Maybe Tutor)
getTutor = getOne "tutor"


------------------------------------------------------------------------------ 
-- | Generic create function.
create :: (MongoIO a) => Collection -> a -> AppHandler String
create name object = 
    fmap (either (const "0") (show))
         (eitherWithDB $ insert name $ new object)

------------------------------------------------------------------------------ 
-- | Specific setters.

createAssignment :: AssignmentValues -> AppHandler AssignmentId
createAssignment (course, task, status, start, end) =
    create "assignment" $ Assignment "" course task status start end
     
createCourse :: CourseValues -> AppHandler CourseId
createCourse (tutor, name, semester, enrol_from, enrol_to, pass_criteria)  =
    create "course" $ Course "" tutor name semester enrol_from enrol_to
                             pass_criteria

createEnrollment :: EnrollmentValues -> AppHandler EnrollmentId
createEnrollment (group, student, time) =
    create "enrollment" $ Enrollment "" group student time

createGroup :: GroupValues -> AppHandler GroupId
createGroup (course, description, capacity)=
    create "courseGroup" $ Group "" course description capacity

createSolution :: SolutionValues -> AppHandler SolutionId
createSolution (task_instance, content, evaluation, result, submission) =
    create "solution" $ Solution "" task_instance content evaluation result
                                  submission

createTask :: TaskValues -> AppHandler TaskId
createTask (tutor, name, type_, signature, scoring_order, created) =
    create "task" $ Task "" tutor name type_ signature scoring_order created

createTaskInstance :: TaskInstanceValues -> AppHandler TaskInstanceId
createTaskInstance (assignment, student, desc, doc, solution, signature) =
    create "task_instance" $ TaskInstance "" assignment student desc doc
                                           solution signature


------------------------------------------------------------------------------ 
-- | Create the necessary database tables, if not already initialized.
-- createTables :: Connection -> IO ()
-- createTables conn = do
