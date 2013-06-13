{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
  , createTables

  ) where

------------------------------------------------------------------------------
import           Control.Monad             (unless)
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import           Database.SQLite.Simple    (Only(Only), Query(Query))
import           Database.SQLite.Simple    (Connection)
import qualified Database.SQLite.Simple    as S
------------------------------------------------------------------------------
-- import           Snap                      ((<$>))
------------------------------------------------------------------------------
import           Application               (AppHandler)
import           Model.Types


------------------------------------------------------------------------------ 
-- | Retrieve.

getAssignmentsByCourse :: CourseId -> AppHandler [Assignment]
getAssignmentsByCourse cid =
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
                          (Only tid)

getTaskInstancesByAssignment :: AssignmentId -> AppHandler [TaskInstance]
getTaskInstancesByAssignment aid =
    undefined

------------------------------------------------------------------------------ 

getTutor :: Integer -> AppHandler (Maybe Tutor)
getTutor tid =
    undefined



------------------------------------------------------------------------------ 
-- | Generic create function.
create  :: (S.ToRow a) => String -> a -> AppHandler Integer
create insertQuery values =
    undefined


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


------------------------------------------------------------------------------ 
-- | Create the necessary database tables, if not already initialized.
createTables :: Connection -> IO ()
createTables conn = do
  createTableIfNotExists conn "assignment" [
      "course INTEGER,"
    , "task INTEGER,"
    , "status,"
    , "start,"
    , "end"]

  createTableIfNotExists conn "course" [
      "tutor INTEGER,"
    , "name,"
    , "semester,"
    , "enrollment_from,"
    , "enrollment_to,"
    , "pass_criteria"]

  createTableIfNotExists conn "enrollment" [
      "group_ INTEGER,"
    , "student INTEGER,"
    , "time"]

  createTableIfNotExists conn "group_" [
      "course INEGER,"
    , "description,"
    , "capacity INTEGER"]

  createTableIfNotExists conn "solution" [
      "task_instance INTEGER,"
    , "content,"
    , "evaluation,"
    , "result,"
    , "submission"]

  createTableIfNotExists conn "task" [
      "tutor INTEGER,"
    , "name,"
    , "type,"
    , "signature,"
    , "scoring_order,"
    , "created"]

  createTableIfNotExists conn "task_instance" [
      "assignment INTEGER,"
    , "student INTEGER,"
    , "description,"
    , "documentation,"
    , "solution,"
    , "signature"]

  tutorCreated <- tableExists conn "tutor"
  createTableIfNotExists conn "tutor" [
      "email"]

  unless tutorCreated . S.execute_ conn . Query $ T.concat [
      "INSERT INTO tutor (email) VALUES ('tutor1@tutor.com');"
    , "INSERT INTO tutor (email) VALUES ('tutor2@tutor.com')"]

  studentCreated <- tableExists conn "student"
  createTableIfNotExists conn "student" [
      "email"]

  unless studentCreated . S.execute_ conn . Query $ T.concat [
       "INSERT INTO student (email) VALUES ('student1@student.com');"
     , "INSERT INTO student (email) VALUES ('student2@student.com');"
     , "INSERT INTO student (email) VALUES ('student3@student.com');"
     , "INSERT INTO student (email) VALUES ('student4@student.com')"]


------------------------------------------------------------------------------ 
-- | 
createTableIfNotExists :: Connection -> String -> [Text] -> IO ()
createTableIfNotExists conn tableName fields = do
  schemaCreated <- tableExists conn tableName
  unless schemaCreated . S.execute_ conn . Query $
       T.concat ["CREATE TABLE ", T.pack tableName,
                 " (id INTEGER PRIMARY KEY,", T.concat fields , ")"]


------------------------------------------------------------------------------ 
-- | 
tableExists :: Connection -> String -> IO Bool
tableExists conn tblName = do
  r <- S.query conn
         "SELECT name FROM sqlite_master WHERE type='table' AND name=?"
         (Only tblName)
  case r of
    [Only (_ :: String)] -> return True
    _                    -> return False
