{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Model.DbAdapter.Sqlite (
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
import           Snap                      ((<$>))
import           Snap.Snaplet.SqliteSimple (query, query_, execute,withSqlite)
------------------------------------------------------------------------------
import           Application               (AppHandler)
import           Model.Types


------------------------------------------------------------------------------ 
-- | Retrieve.

getAssignmentsByCourse :: CourseId -> AppHandler [Assignment]
getAssignmentsByCourse cid =
    query "SELECT * FROM assignment WHERE course = ?" (Only cid)

getAssignmentsByTask :: TaskId -> AppHandler [Assignment]
getAssignmentsByTask tid =
    query "SELECT * FROM assignment WHERE task = ?" (Only tid)

------------------------------------------------------------------------------ 

getCourse :: CourseId -> AppHandler (Maybe Course)
getCourse cid =
    listToMaybe <$> query "SELECT * FROM course WHERE id = ?" (Only cid)

getAllCourses :: AppHandler [Course]
getAllCourses =
    query_ "SELECT * FROM course"

getCoursesByTutor :: TutorId -> AppHandler [Course]
getCoursesByTutor tid =
  query "SELECT * FROM course WHERE tutor = ?" (Only tid)

------------------------------------------------------------------------------ 

getEnrollmentsByStudent :: StudentId -> AppHandler [Enrollment]
getEnrollmentsByStudent tid =
  query "SELECT * FROM enrollment WHERE student = ?" (Only tid)

------------------------------------------------------------------------------ 

getGroups :: [GroupId] -> AppHandler [Group]
getGroups gids = do
    filter (\g -> groupId g `elem` gids) <$> query_ "SELECT * FROM group_"

getGroupsByCourse :: CourseId -> AppHandler [Group]
getGroupsByCourse tid =
  query "SELECT * FROM group_ WHERE course = ?" (Only tid)

------------------------------------------------------------------------------ 

getSolutionsByTaskInstance :: TaskInstanceId -> AppHandler [Solution]
getSolutionsByTaskInstance tid =
    query "SELECT * FROM solution WHERE task_instance = ?" (Only tid)

getLastSolutionsByTaskInstance :: TaskInstanceId -> AppHandler (Maybe Solution)
getLastSolutionsByTaskInstance tid =
    listToMaybe <$> query (Query $ T.concat [
        "SELECT * FROM solution WHERE task_instance = ? "
      , "ORDER BY submission DESC"]) (Only tid)

------------------------------------------------------------------------------ 

getStudent :: StudentId -> AppHandler (Maybe Student)
getStudent sid =
    listToMaybe <$> query "SELECT * FROM student WHERE id = ?" (Only sid)

------------------------------------------------------------------------------ 

getTask :: TaskId -> AppHandler (Maybe Task)
getTask tid =
    listToMaybe <$> query "SELECT * FROM task WHERE id = ?" (Only tid)

getTasksByTutor :: TutorId -> AppHandler [Task]
getTasksByTutor tid =
    query "SELECT * FROM task WHERE tutor = ?" (Only tid)

------------------------------------------------------------------------------ 

getTaskInstance :: TaskInstanceId -> AppHandler (Maybe TaskInstance)
getTaskInstance tid =
    listToMaybe <$> query "SELECT * FROM task_instance WHERE id = ?"
                          (Only tid)

getTaskInstancesByAssignment :: AssignmentId -> AppHandler [TaskInstance]
getTaskInstancesByAssignment aid =
    query "SELECT * FROM task_instance WHERE assignment = ?" (Only aid)

------------------------------------------------------------------------------ 

getTutor :: Integer -> AppHandler (Maybe Tutor)
getTutor tid =
    listToMaybe <$> query "SELECT * FROM tutor WHERE id = ?" (Only tid)



------------------------------------------------------------------------------ 
-- | Generic create function.
create  :: (S.ToRow a) => String -> a -> AppHandler Integer
create insertQuery values = do
    _ <- flip execute values $ Query $ T.pack insertQuery
    fromIntegral <$> (withSqlite S.lastInsertRowId)


------------------------------------------------------------------------------ 
-- | Specific setters.

createAssignment :: AssignmentValues -> AppHandler AssignmentId
createAssignment = create $ concat
    [ "INSERT INTO assignment ("
    , "course, task, status, start, end"
    , ") VALUES (?,?,?,?,?)"]
     
createCourse :: CourseValues -> AppHandler CourseId
createCourse = create $ concat
    [ "INSERT INTO course ("
    , "tutor, name, semester, enrollment_from, enrollment_to, pass_criteria"
    , ") VALUES (?,?,?,?,?,?)"]

createEnrollment :: EnrollmentValues -> AppHandler EnrollmentId
createEnrollment = create $ concat
    [ "INSERT INTO enrollment ("
    , "group_, student, time"
    , ") VALUES (?,?,?)"]

createGroup :: GroupValues -> AppHandler GroupId
createGroup = create $ concat
    [ "INSERT INTO group_ ("
    , "course, description, capacity"
    , ") VALUES (?,?,?)"]

createSolution :: SolutionValues -> AppHandler SolutionId
createSolution = create $ concat
    [ "INSERT INTO solution ("
    , "task_instance, content, evaluation, result, submission"
    , ") VALUES (?,?,?,?,?)"]

createTask :: TaskValues -> AppHandler TaskId
createTask = create $ concat
    [ "INSERT INTO task ("
    , "tutor, name, type, signature, scoring_order, created"
    , ") VALUES (?,?,?,?,?,?)"]

createTaskInstance :: TaskInstanceValues -> AppHandler TaskInstanceId
createTaskInstance = create $ concat
    [ "INSERT INTO task_instance ("
    , "assignment, student, description, documentation, solution, signature"
    , ") VALUES (?,?,?,?,?,?)"]


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
