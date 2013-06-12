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
  , initUsers

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

get :: (App -> IORef (Map Integer a)) -> AppHandler (Map Integer a)
get ioRef = do
    objectRef <- gets ioRef
    liftIO $ readIORef objectRef

list :: (App -> IORef (Map Integer a)) -> AppHandler [a]
list = fmap Map.elems . get

create :: (Indexable a) => (App -> IORef (Map Integer a)) -> a
       -> AppHandler Integer
create ioRef object = do
    objectRef <- gets ioRef
    objects   <- liftIO $ readIORef objectRef
    let newObject   = constructOrCopy object objects
        newObjects  = Map.insert (iid newObject) newObject objects
    liftIO $ writeIORef objectRef newObjects
    return $ iid newObject
  where
    constructOrCopy obj objs
      | iid object == 0 = setId object (getNextId objs)
      | otherwise       = obj


------------------------------------------------------------------------------ 
-- | Retrieve.

getAssignmentsByCourse :: CourseId -> AppHandler [Assignment]
getAssignmentsByCourse cid =
    filter (\a -> assignmentCourse a == cid) <$> list _assignments

getAssignmentsByTask :: TaskId -> AppHandler [Assignment]
getAssignmentsByTask tid =
    filter (\a -> assignmentTask a == tid) <$> list _assignments

------------------------------------------------------------------------------ 

getCourse :: CourseId -> AppHandler (Maybe Course)
getCourse cid = Map.lookup cid <$> get _courses

getAllCourses :: AppHandler [Course]
getAllCourses = list _courses

getCoursesByTutor :: TutorId -> AppHandler [Course]
getCoursesByTutor tid =
    filter (\c -> courseTutor c == tid) <$> list _courses

------------------------------------------------------------------------------ 

getEnrollmentsByStudent :: StudentId -> AppHandler [Enrollment]
getEnrollmentsByStudent sid =
    filter (\e -> enrollmentStudent e == sid) <$> list _enrollments

------------------------------------------------------------------------------ 

getGroups :: [GroupId] -> AppHandler [Group]
getGroups gids = do
    filter (\g -> groupId g `elem` gids) <$> list _groups

getGroupsByCourse :: CourseId -> AppHandler [Group]
getGroupsByCourse cid =
    filter (\a -> groupCourse a == cid) <$> list _groups

------------------------------------------------------------------------------ 

getSolutionsByTaskInstance :: TaskInstanceId -> AppHandler [Solution]
getSolutionsByTaskInstance tid =
    filter (\s -> solutionTaskInstance s == tid) <$> list _solutions

getLastSolutionsByTaskInstance :: TaskInstanceId -> AppHandler (Maybe Solution)
getLastSolutionsByTaskInstance tid =
    listToMaybe <$> filter (\s -> solutionTaskInstance s == tid)
                <$> list _solutions

------------------------------------------------------------------------------ 

getStudent :: StudentId -> AppHandler (Maybe Student)
getStudent sid = Map.lookup sid <$> get _students

------------------------------------------------------------------------------ 

getTask :: TaskId -> AppHandler (Maybe Task)
getTask tid = Map.lookup tid <$> get _tasks

getTasksByTutor :: TutorId -> AppHandler [Task]
getTasksByTutor tid =
    filter (\t -> taskTutor t == tid) <$> list _tasks

------------------------------------------------------------------------------ 

getTaskInstance :: TaskInstanceId -> AppHandler (Maybe TaskInstance)
getTaskInstance tid = Map.lookup tid <$> get _taskInstances

getTaskInstancesByAssignment :: AssignmentId -> AppHandler [TaskInstance]
getTaskInstancesByAssignment aid =
    filter (\t -> taskInstanceAssignment t == aid) <$> list _taskInstances

------------------------------------------------------------------------------ 

getTutor :: TutorId -> AppHandler (Maybe Tutor)
getTutor tid = Map.lookup tid <$> get _tutors


------------------------------------------------------------------------------ 
-- | Utils.
listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe xs = Just $ last xs


------------------------------------------------------------------------------ 
-- | Specific setters.

createAssignment :: AssignmentValues -> AppHandler AssignmentId
createAssignment (course, task, status, start, end) =
    create _assignments $ Assignment 0 course task status start end

createCourse :: CourseValues -> AppHandler CourseId
createCourse (tutor, name, semester, enrol_from, enrol_to, pass_criteria) =
    create _courses $ Course 0 tutor name semester enrol_from enrol_to
                              pass_criteria

createEnrollment :: EnrollmentValues -> AppHandler EnrollmentId
createEnrollment (group, student, time) =
    create _enrollments $ Enrollment 0 group student time

createGroup :: GroupValues -> AppHandler GroupId
createGroup (course, description, capacity)=
    create _groups $ Group 0 course description capacity

createSolution :: SolutionValues -> AppHandler SolutionId
createSolution (task_instance, content, evaluation, result, submission) =
    create _solutions $ Solution 0 task_instance content evaluation result
                                  submission

createTask :: TaskValues -> AppHandler TaskId
createTask (tutor, name, type_, signature, scoring_order, created) =
    create _tasks $ Task 0 tutor name type_ signature scoring_order created

createTaskInstance :: TaskInstanceValues -> AppHandler TaskInstanceId
createTaskInstance (assignment, student, desc, doc, solution, signature) =
    create _taskInstances $ TaskInstance 0 assignment student desc doc
                                           solution signature


------------------------------------------------------------------------------ 
-- | Set up.
initUsers :: (IORef (Map Integer Tutor)) -> (IORef (Map Integer Student))
          -> IO ()
initUsers tutorRef studentRef = do
    writeIORef tutorRef tutors
    writeIORef studentRef students
  where
    tutors = Map.fromList [
        (1, Tutor 1 "tutor1@htwk-leipzig.de")
      , (2, Tutor 2 "tutor2@htwk-leipzig.de")
      ]
    students = Map.fromList [
        (1, Student 1 "student1@htwk-leipzig.de")
      , (2, Student 2 "student2@htwk-leipzig.de")
      , (3, Student 3 "student3@htwk-leipzig.de")
      , (4, Student 4 "student4@htwk-leipzig.de")
      ]
