module Database.Adapter.IORef (
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
  , getLastSolutionByTaskInstance

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
import           Model.Datatypes
import           Model.Extensions.Indexable


------------------------------------------------------------------------------ 
-- | Generic getter and setter.

get :: (App -> IORef (Map String a)) -> AppHandler (Map String a)
get ioRef = do
    objectRef <- gets ioRef
    liftIO $ readIORef objectRef

list :: (App -> IORef (Map String a)) -> AppHandler [a]
list = fmap Map.elems . get

create :: (Indexable a) => (App -> IORef (Map String a)) -> a
       -> AppHandler String
create ioRef object = do
    objectRef <- gets ioRef
    objects   <- liftIO $ readIORef objectRef
    let newObject   = constructOrCopy object objects
        newObjects  = Map.insert (iid newObject) newObject objects
    liftIO $ writeIORef objectRef newObjects
    return $ iid newObject
  where
    constructOrCopy obj objs
      | iid object == "" = setId object (getNextId objs)
      | otherwise        = obj
 

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

getLastSolutionByTaskInstance :: TaskInstanceId -> AppHandler (Maybe Solution)
getLastSolutionByTaskInstance tid =
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
-- | Return the highest found index (id) + 1 from a list of indexable DTs.
getNextId :: (Indexable a) => Map String a -> String
getNextId m | Map.null m = "1"
            | otherwise  = show $ maximum (map read (Map.keys m)::[Int]) + 1


------------------------------------------------------------------------------ 
-- | Specific setters.

createAssignment :: AssignmentValues -> AppHandler AssignmentId
createAssignment (course, task, status, start, end) =
    create _assignments $ Assignment "" course task status start end

createCourse :: CourseValues -> AppHandler CourseId
createCourse (tutor, name, semester, enrol_from, enrol_to, pass_criteria) =
    create _courses $ Course "" tutor name semester enrol_from enrol_to
                              pass_criteria

createEnrollment :: EnrollmentValues -> AppHandler EnrollmentId
createEnrollment (group, student, time) =
    create _enrollments $ Enrollment "" group student time

createGroup :: GroupValues -> AppHandler GroupId
createGroup (course, description, capacity)=
    create _groups $ Group "" course description capacity

createSolution :: SolutionValues -> AppHandler SolutionId
createSolution (task_instance, content, evaluation, result, submission) =
    create _solutions $ Solution "" task_instance content evaluation result
                                  submission

createTask :: TaskValues -> AppHandler TaskId
createTask (tutor, name, type_, signature, scoring_order, created) =
    create _tasks $ Task "" tutor name type_ signature scoring_order created

createTaskInstance :: TaskInstanceValues -> AppHandler TaskInstanceId
createTaskInstance (assignment, student, desc, doc, solution, signature) =
    create _taskInstances $ TaskInstance "" assignment student desc doc
                                            solution signature


------------------------------------------------------------------------------ 
-- | Set up.
initUsers :: (IORef (Map String Tutor)) -> (IORef (Map String Student))
          -> IO ()
initUsers tutorRef studentRef = do
    writeIORef tutorRef tutors
    writeIORef studentRef students
  where
    tutors = Map.fromList [
        ("51c8235ef4d13fc80f76c462"
        , Tutor "51c8235ef4d13fc80f76c462" "tutor1@htwk-leipzig.de")
      ]
    students = Map.fromList [
        ("51c83fd80e19e3dfb1bca0ae"
        , Student "51c83fd80e19e3dfb1bca0ae" "student1@htwk-leipzig.de")
      , ("51c83fd80e19e3dfb1bca0af"
        , Student "51c83fd80e19e3dfb1bca0af" "student2@htwk-leipzig.de")
      , ("51c83fd80e19e3dfb1bca0b0"
        , Student "51c83fd80e19e3dfb1bca0b0" "student3@htwk-leipzig.de")
      , ("51c83fd80e19e3dfb1bca0b1"
        , Student "51c83fd80e19e3dfb1bca0b1" "student4@htwk-leipzig.de")
      ]
