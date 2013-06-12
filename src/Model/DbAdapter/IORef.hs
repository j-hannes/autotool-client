module Model.DbAdapter.IORef (
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
  , getSolutionsByTaskInstance
  , getTasksByTutor
  , getTaskInstancesByAssignment

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
  , createFiles

  ) where

------------------------------------------------------------------------------
import           Data.IORef           (IORef, readIORef, writeIORef)
import           Data.Map             (Map)
import qualified Data.Map             as Map
------------------------------------------------------------------------------
import           Snap                 (gets, liftIO, (<$>))
------------------------------------------------------------------------------
import           Application          (App(..), AppHandler)
import           Model.DbAdapter.Base (getNextId)
import           Model.Indexable
import           Model.Types


------------------------------------------------------------------------------ 
-- | Generic getter and setter.

get :: (App -> IORef (Map Integer a)) -> Integer -> AppHandler (Maybe a)
get ioRef oid = do
    objectRef <- gets ioRef
    objectMap <- liftIO $ readIORef objectRef
    return $ Map.lookup oid objectMap

list :: (App -> IORef (Map Integer a)) -> AppHandler [a]
list ioRef = do
    objectRef <- gets ioRef
    liftIO $ Map.elems <$> readIORef objectRef

create :: (Indexable a) => (App -> IORef (Map Integer a)) -> a -> AppHandler a
create ioRef object = do
    objectRef <- gets ioRef
    objects   <- liftIO $ readIORef objectRef
    let newObject   = constructOrCopy object objects
        newObjects  = Map.insert (iid newObject) newObject objects
    liftIO $ writeIORef objectRef newObjects
    return $ newObject
  where
    constructOrCopy obj objs
      | iid object == 0 = setId object (getNextId objs)
      | otherwise       = obj


------------------------------------------------------------------------------ 
-- | Specific getters.

getAssignment :: Integer ->  AppHandler (Maybe Assignment)
getAssignment = get _assignments

getCourse :: Integer -> AppHandler (Maybe Course)
getCourse = get _courses

getCourses :: AppHandler [Course]
getCourses = list _courses

getEnrollment :: Integer -> AppHandler (Maybe Enrollment)
getEnrollment = get _enrollments

getGroup :: Integer -> AppHandler (Maybe Group)
getGroup = get _groups

getSolution :: Integer -> AppHandler (Maybe Solution)
getSolution = get _solutions

getStudent :: Integer -> AppHandler (Maybe Student)
getStudent = get _students

getTask :: Integer -> AppHandler (Maybe Task)
getTask = get _tasks

getTutor :: Integer -> AppHandler (Maybe Tutor)
getTutor = get _tutors

getTaskInstance :: Integer -> AppHandler (Maybe TaskInstance)
getTaskInstance = get _taskInstances


------------------------------------------------------------------------------ 
-- | Specific setters.

createAssignment :: Assignment -> AppHandler Assignment
createAssignment = create _assignments

createCourse :: Course -> AppHandler Course
createCourse = create _courses

createEnrollment :: Enrollment -> AppHandler Enrollment
createEnrollment = create _enrollments

createGroup :: Group -> AppHandler Group
createGroup = create _groups

createSolution :: Solution -> AppHandler Solution
createSolution = create _solutions

createStudent :: Student -> AppHandler Student
createStudent = create _students

createTask :: Task -> AppHandler Task
createTask = create _tasks

createTutor :: Tutor -> AppHandler Tutor
createTutor = create _tutors

createTaskInstance :: TaskInstance -> AppHandler TaskInstance
createTaskInstance = create _taskInstances
