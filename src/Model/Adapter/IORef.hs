module Model.Adapter.IORef where

------------------------------------------------------------------------------
import Data.IORef         (IORef, readIORef, writeIORef)
------------------------------------------------------------------------------
import Snap               (gets, liftIO)
------------------------------------------------------------------------------
import Application        (App(..), AppHandler)
import Model.Adapter.Base (getNextId)
import Model.Indexable
import Model.Types


------------------------------------------------------------------------------ 
-- | Generic getter and setter.

get :: (App -> IORef [a]) -> AppHandler [a]
get ioRef = do
    assignmentRef <- gets ioRef
    liftIO $ readIORef assignmentRef

create :: (Indexable a) => (App -> IORef [a]) -> a -> AppHandler a
create ioRef object = do
    objectRef <- gets ioRef
    objects   <- liftIO $ readIORef objectRef
    let nextId     = getNextId 0 objects
        newObject  = setId object nextId
        newObjects = newObject : objects
    liftIO $ writeIORef objectRef newObjects
    return $ newObject


------------------------------------------------------------------------------ 
-- | Specific getters.

getAssignments   :: AppHandler [Assignment]
getAssignments   = get _assignments

getCourses       :: AppHandler [Course]
getCourses       = get _courses

getEnrollments   :: AppHandler [Enrollment]
getEnrollments   = get _enrollments

getGroups        :: AppHandler [Group]
getGroups        = get _groups

getSolutions     :: AppHandler [Solution]
getSolutions     = get _solutions

getTasks         :: AppHandler [Task]
getTasks         = get _tasks

getTaskInstances :: AppHandler [TaskInstance]
getTaskInstances = get _taskInstances


------------------------------------------------------------------------------ 
-- | Specific setters.

createAssignment   :: Assignment -> AppHandler Assignment
createAssignment   = create _assignments

createCourse       :: Course -> AppHandler Course
createCourse       = create _courses

createEnrollment   :: Enrollment -> AppHandler Enrollment
createEnrollment   = create _enrollments

createGroup        :: Group -> AppHandler Group
createGroup        = create _groups

createSolution     :: Solution -> AppHandler Solution
createSolution     = create _solutions

createTask         :: Task -> AppHandler Task
createTask         = create _tasks

createTaskInstance :: TaskInstance -> AppHandler TaskInstance
createTaskInstance = create _taskInstances
