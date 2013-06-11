module Model.DbAdapter.IORef where

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

put :: (Indexable a) => (App -> IORef (Map Integer a)) -> a -> AppHandler a
put ioRef object = do
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

getAssignment   :: Integer ->  AppHandler (Maybe Assignment)
getAssignment   = get _assignments

getCourse       :: Integer -> AppHandler (Maybe Course)
getCourse       = get _courses

getCourses      :: AppHandler [Course]
getCourses      = list _courses

getEnrollment   :: Integer -> AppHandler (Maybe Enrollment)
getEnrollment   = get _enrollments

getGroup        :: Integer -> AppHandler (Maybe Group)
getGroup        = get _groups

getSolution     :: Integer -> AppHandler (Maybe Solution)
getSolution     = get _solutions

getStudent        :: Integer -> AppHandler (Maybe Student)
getStudent        = get _students

getTask         :: Integer -> AppHandler (Maybe Task)
getTask         = get _tasks

getTutor        :: Integer -> AppHandler (Maybe Tutor)
getTutor        = get _tutors

getTaskInstance :: Integer -> AppHandler (Maybe TaskInstance)
getTaskInstance = get _taskInstances


------------------------------------------------------------------------------ 
-- | Specific setters.

putAssignment   :: Assignment -> AppHandler Assignment
putAssignment   = put _assignments

putCourse       :: Course -> AppHandler Course
putCourse       = put _courses

putEnrollment   :: Enrollment -> AppHandler Enrollment
putEnrollment   = put _enrollments

putGroup        :: Group -> AppHandler Group
putGroup        = put _groups

putSolution     :: Solution -> AppHandler Solution
putSolution     = put _solutions

putStudent         :: Student -> AppHandler Student
putStudent         = put _students

putTask         :: Task -> AppHandler Task
putTask         = put _tasks

putTutor         :: Tutor -> AppHandler Tutor
putTutor         = put _tutors

putTaskInstance :: TaskInstance -> AppHandler TaskInstance
putTaskInstance = put _taskInstances
