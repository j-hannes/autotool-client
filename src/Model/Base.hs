module Model.Base where

------------------------------------------------------------------------------
import Control.Monad           (forM)
import Data.Maybe              (catMaybes)
import Snap                    (liftIO, (<$>))
------------------------------------------------------------------------------
import Application             (AppHandler)
import Autotool.Client         as Autotool
------------------------------------------------------------------------------
import Model.Adapter.FileStore as Adapter
import Model.Types


------------------------------------------------------------------------------
-- simple retrieve functions
------------------------------------------------------------------------------

getAssignment :: AssignmentId -> AppHandler (Maybe Assignment)
getAssignment = Adapter.getAssignment

getAssignments :: [AssignmentId] -> AppHandler [Assignment]
getAssignments aids = catMaybes <$> mapM Adapter.getAssignment aids

------------------------------------------------------------------------------

getCourse :: CourseId -> AppHandler (Maybe Course)
getCourse = Adapter.getCourse

getCourses :: [CourseId] -> AppHandler [Course]
getCourses cids = catMaybes <$> mapM Adapter.getCourse cids

getAllCourses :: AppHandler [Course]
getAllCourses = Adapter.getCourses

------------------------------------------------------------------------------

getEnrollments :: [EnrollmentId] -> AppHandler [Enrollment]
getEnrollments eids = catMaybes <$> mapM Adapter.getEnrollment eids

------------------------------------------------------------------------------

getGroup :: GroupId -> AppHandler (Maybe Group)
getGroup = Adapter.getGroup

getGroups :: [GroupId] -> AppHandler [Group]
getGroups gids = catMaybes <$> mapM Adapter.getGroup gids

------------------------------------------------------------------------------

getTaskInstance :: TaskInstanceId -> AppHandler (Maybe TaskInstance)
getTaskInstance = Adapter.getTaskInstance

getTaskInstances :: [TaskInstanceId] -> AppHandler [TaskInstance]
getTaskInstances tiids = catMaybes <$> mapM Adapter.getTaskInstance tiids

------------------------------------------------------------------------------

getSolution :: SolutionId -> AppHandler (Maybe Solution)
getSolution = Adapter.getSolution

getSolutions :: [SolutionId] -> AppHandler [Solution]
getSolutions tiids = catMaybes <$> mapM Adapter.getSolution tiids

------------------------------------------------------------------------------

getStudent :: StudentId -> AppHandler (Maybe Student)
getStudent = Adapter.getStudent

------------------------------------------------------------------------------

getTask :: TaskId -> AppHandler (Maybe Task)
getTask = Adapter.getTask

getTasks :: [TaskId] -> AppHandler [Task]
getTasks tids = catMaybes <$> mapM Adapter.getTask tids

------------------------------------------------------------------------------

getTutor :: TutorId -> AppHandler (Maybe Tutor)
getTutor = Adapter.getTutor



------------------------------------------------------------------------------
-- complex retrieve functions
------------------------------------------------------------------------------

getEnrollableCourses :: Student -> AppHandler [Course]
getEnrollableCourses student = do
    groups      <- Model.Base.getEnrolledGroups student
    courses     <- Model.Base.getAllCourses
    return $ filter (\c -> not $ courseId c `elem` (map groupCourseId groups))
               courses

getEnrolledGroups :: Student -> AppHandler [Group]
getEnrolledGroups student = do
    enrollments <- Model.Base.getEnrollments (studentEnrollments student)
    Model.Base.getGroups (map enrollmentGroupId enrollments)

------------------------------------------------------------------------------

getTasksWithAssignmentCount :: Tutor -> AppHandler [(Task, Int)]
getTasksWithAssignmentCount tutor = do
    tasks <- Model.Base.getTasks (tutorTasks tutor)
    forM tasks filterTasks
  where
    filterTasks task = do
      assignments <- mapM Adapter.getAssignment (taskAssignments task)
      return (task, length assignments)

------------------------------------------------------------------------------

getCachedTaskInstance :: Task -> Student -> AppHandler TaskInstance
getCachedTaskInstance task student = do
    taskInstances <- Model.Base.getTaskInstances (taskTaskInstances task)
    let taskInstances' = filterInstances taskInstances
    if null taskInstances'
      then do
        (desc, sol, doc, sig) <- liftIO $ Autotool.getTaskInstance
                                            (taskSignature task) (show sid)
        ti <- Model.Base.putTaskInstance $ TaskInstance 0 (taskId task) sid []
                                                        desc (show doc) sol sig
        _  <- Model.Base.putTask $ task {taskTaskInstances =
                taskInstanceId ti : taskTaskInstances task}

        return ti
      else
        return $ head taskInstances'
  where
    sid             = studentId student
    filterInstances = filter (\ti -> taskInstanceStudentId ti == sid)

------------------------------------------------------------------------------

{- eeeeeeeh ... impossible! -> taskInstances need a relationship to assignments
getAssignmentSubmissions :: Assignment -> AppHandler Int
getAssignmentSubmissions assignment = do
  
    taskInstances   <- lift $ Model.getTaskInstances (taskTaskInstances task)
-}

------------------------------------------------------------------------------
-- put functions
------------------------------------------------------------------------------

putAssignment :: Assignment -> AppHandler Assignment
putAssignment = Adapter.putAssignment

putCourse :: Course -> AppHandler Course
putCourse = Adapter.putCourse

putEnrollment :: Enrollment -> AppHandler Enrollment
putEnrollment = Adapter.putEnrollment

putGroup :: Group -> AppHandler Group
putGroup = Adapter.putGroup

putSolution :: Solution -> AppHandler Solution
putSolution = Adapter.putSolution

putStudent :: Student -> AppHandler Student
putStudent = Adapter.putStudent

putTask :: Task -> AppHandler Task
putTask = Adapter.putTask

putTutor :: Tutor -> AppHandler Tutor
putTutor = Adapter.putTutor

putTaskInstance :: TaskInstance -> AppHandler TaskInstance
putTaskInstance = Adapter.putTaskInstance


------------------------------------------------------------------------------
-- new functions
------------------------------------------------------------------------------

newTutor :: TutorId -> AppHandler Tutor
newTutor tid = Model.Base.putTutor $ Tutor tid [] []

newStudent :: StudentId -> AppHandler Student
newStudent sid = Model.Base.putStudent $ Student sid [] []
