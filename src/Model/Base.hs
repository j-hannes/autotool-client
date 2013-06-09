module Model.Base where

------------------------------------------------------------------------------
import Control.Monad       (forM)
import Data.Maybe          (catMaybes, fromJust)
import Snap                (liftIO, (<$>))
------------------------------------------------------------------------------
import Application         (AppHandler)
import Autotool.Client     as Autotool
------------------------------------------------------------------------------
import Model.Adapter.IORef as Adapter
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

getStudentGroups :: Student -> AppHandler [Group]
getStudentGroups student = do
    enrollments <- Model.Base.getEnrollments (studentEnrollments student)
    Model.Base.getGroups (map enrollmentGroupId enrollments)

getStudentCourses :: Student -> AppHandler [Course]
getStudentCourses student = do
    groups <- Model.Base.getStudentGroups student
    Model.Base.getCourses (map groupCourseId groups)

getEnrollableCourses :: Student -> AppHandler [(Course, [Group])]
getEnrollableCourses student = do
    courses        <- Model.Base.getAllCourses
    studentCourses <- Model.Base.getStudentCourses student
    let courses' = filter (\c -> not $ courseId c `elem` (map courseId studentCourses)) courses
    forM courses' attachGroups
  where
    attachGroups course = do 
      groups <- Model.Base.getGroups (courseGroups course) 
      return (course, groups)

------------------------------------------------------------------------------

getStudentGroupBundles :: Student -> AppHandler [GroupBundle]
getStudentGroupBundles student = do
    groups <- Model.Base.getStudentGroups student
    forM groups filterGroups
  where
    filterGroups group = do
      course      <- fromJust <$> Model.Base.getCourse (groupCourseId group)
      assignments <- Model.Base.getAssignments (courseAssignments course) 
      bundle      <- forM assignments filterAssignments
      return (group, course, bundle)
    filterAssignments assignment = do
      task         <- fromJust <$> Model.Base.getTask
                        (assignmentTaskId assignment)
      taskInstance <- Model.Base.getCachedTaskInstance task student
      return (assignment, task, taskInstance)

getTutorCourseBundles :: Tutor -> AppHandler [CourseBundle]
getTutorCourseBundles tutor = do
    courses <- Model.Base.getCourses (tutorCourses tutor)
    forM courses filterCourses
  where
    filterCourses course = do
      assignments <- Model.Base.getAssignments (courseAssignments course)
      bundle      <- forM assignments filterAssignments
      return (course, bundle)
    filterAssignments assignment = do
      task <- fromJust <$> Model.Base.getTask (assignmentTaskId assignment)
      return (assignment, task)

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
        Model.Base.putTaskInstance $ TaskInstance 0 (taskId task) sid [] desc sol (show doc) sig
      else return $ head taskInstances'
  where
    sid             = studentId student
    filterInstances = filter (\ti -> taskInstanceStudentId ti == sid)



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
