module Model.Base where

import Control.Monad                      (forM)
import Data.Maybe                         (fromJust, isJust)
import Data.Time                          (UTCTime)
import Snap                               (liftIO, (<$>))

import Application                        (AppHandler)
import Autotool.Client                    as Autotool
import Autotool.Client.Types.ScoringOrder

import Model.Adapter.IORef                as Adapter
import Model.Types


------------------------------------------------------------------------------
-- simple accessor functions

getAssignments :: [AssignmentId] -> AppHandler [Assignment]
getAssignments aids = do
  assignments <- Adapter.getAssignments
  return $ filter (\a -> assignmentCourseId a `elem` aids) assignments

getCourses :: [CourseId] -> AppHandler [Course]
getCourses cids = do
  courses <- Adapter.getCourses
  return $ filter (\c -> courseId c `elem` cids) courses

getCoursesExcept :: [CourseId] -> AppHandler [Course]
getCoursesExcept cids = do
  courses <- Adapter.getCourses
  return $ filter (\c -> not $ courseId c `elem` cids) courses

getGroups :: [GroupId] -> AppHandler [Group]
getGroups cids = do
  groups <- Adapter.getGroups
  return $ filter (\c -> groupId c `elem` cids) groups

getTaskInstanceById :: TaskInstanceId -> AppHandler TaskInstance
getTaskInstanceById tiid = do
  taskInstances <- Adapter.getTaskInstances
  return $ head $ filter (\t -> taskInstanceId t == tiid) taskInstances

getTaskInstanceForTask :: TaskId -> StudentId -> AppHandler (Maybe TaskInstance)
getTaskInstanceForTask tid sid = do
  taskInstances <- Adapter.getTaskInstances
  let ti' = filter (\ti -> taskInstanceTaskId ti == tid &&
                           taskInstanceStudentId ti == sid) taskInstances
  if null ti' then return Nothing else return $ Just $ head ti'

getTask :: TaskId -> AppHandler Task
getTask tid = do
  tasks <- Adapter.getTasks
  return $ head $ filter (\t -> taskId t == tid) tasks

getTasks :: [TaskId] -> AppHandler [Task]
getTasks tids = do
  tasks <- Adapter.getTasks
  return $ filter (\t -> taskId t `elem` tids) tasks

getTasksByTutorId :: TutorId -> AppHandler [Task]
getTasksByTutorId tid = do
  tasks <- Adapter.getTasks
  return $ filter (\t -> taskTutorId t == tid) tasks


------------------------------------------------------------------------------
-- complex accessor functions

getCoursesByStudentId :: StudentId -> AppHandler [Course]
getCoursesByStudentId sid = do
    groups <- Model.Base.getGroupsByStudentId sid
    Model.Base.getCourses $ map groupCourseId groups

getGroupBundlesByStudentId :: StudentId -> AppHandler [GroupBundle]
getGroupBundlesByStudentId sid = do
    groups <- Model.Base.getGroupsByStudentId sid
    forM groups filterGroups
  where
    filterGroups group = do
      course      <- head <$> Model.Base.getCourses [groupCourseId group]
      assignments <- Model.Base.getAssignments (courseAssignments course) 
      bundle      <- forM assignments filterAssignments
      return (group, course, bundle)
    filterAssignments assignment = do
      task         <- Model.Base.getTask (assignmentTaskId assignment)
      taskInstance <- Model.Base.getCachedTaskInstance task sid
      return (assignment, task, taskInstance)

getCourseBundlesByTutor :: Tutor -> AppHandler [CourseBundle]
getCourseBundlesByTutor tutor = do
    courses <- Model.Base.getCourses (tutorCourses tutor)
    forM courses filterCourses
  where
    filterCourses course = do
      assignments <- Model.Base.getAssignments (courseAssignments course)
      bundle      <- forM assignments filterAssignments
      return (course, bundle)
    filterAssignments assignment = do
      task <- getTask (assignmentTaskId assignment)
      return (assignment, task)

getGroupsByStudentId :: StudentId -> AppHandler [Group]
getGroupsByStudentId sid = do
    enrollments <- Adapter.getEnrollments
    let e' = filter (\e -> enrollmentStudentId e == sid) enrollments
    Model.Base.getGroups $ map enrollmentGroupId e'

getTasksWithAssignmentCount :: TutorId -> AppHandler [(Task, Int)]
getTasksWithAssignmentCount tid = do
    tasks       <- Adapter.getTasks
    let t' = filter (\t -> taskTutorId t == tid) tasks
    assignments <- Adapter.getAssignments
    forM t' (filterTasks assignments)
  where
    filterTasks assignments task = do
      let a' = filter (\a -> assignmentTaskId a == taskId task) assignments
      return (task, length a')

getCoursesWithEnrollableGroups :: StudentId -> AppHandler [(Course, [Group])]
getCoursesWithEnrollableGroups sid = do
    courses  <- Model.Base.getCoursesByStudentId sid
    courses' <- Model.Base.getCoursesExcept (map courseId courses)
    groups  <- Adapter.getGroups
    forM courses' (filterCourses groups)
  where
    filterCourses groups course = do 
      let g' = filter (\g -> groupCourseId g == courseId course) groups
      return (course, g')

getCachedTaskInstance :: Task -> StudentId -> AppHandler TaskInstance
getCachedTaskInstance task sid = do
  loadedTaskInstance <- Model.Base.getTaskInstanceForTask (taskId task) sid
  if isJust loadedTaskInstance
    then return $ fromJust loadedTaskInstance
    else do
      (desc, sol, doc, sig) <- liftIO $ Autotool.getTaskInstance
                                          (taskSignature task) (show sid)
      Model.Base.createTaskInstance (taskId task) sid desc sol (show doc) sig


------------------------------------------------------------------------------
-- create functions

createAssignment :: CourseId -> TaskId -> Status -> UTCTime -> UTCTime
                 -> AppHandler Assignment
createAssignment cid tid sts start end =
    Adapter.createAssignment $ Assignment 0 cid tid sts start end

createCourse :: TutorId -> String -> String -> Maybe UTCTime -> Maybe UTCTime
             -> Double -> AppHandler Course
createCourse tid name sem enrStart enrEnd pass =
    Adapter.createCourse $ Course 0 tid [] [] name sem enrStart enrEnd pass

createEnrollment :: GroupId -> StudentId -> UTCTime -> AppHandler Enrollment
createEnrollment gid sid time =
    Adapter.createEnrollment $ Enrollment 0 gid sid time

createGroup :: CourseId -> String -> Int -> AppHandler Group
createGroup cid desc cap =
    Adapter.createGroup $ Group 0 cid [] desc cap

createSolution :: TaskInstanceId -> String -> String -> Maybe Result -> UTCTime
               -> AppHandler Solution
createSolution tid cont eval res time =
    Adapter.createSolution $ Solution 0 tid cont eval res time

createTask :: TutorId -> String -> String -> String -> ScoringOrder -> UTCTime
           -> AppHandler Task
createTask tid name ttpe sig so time =
    Adapter.createTask $ Task 0 tid [] [] name ttpe sig so time

createTaskInstance :: TaskId -> StudentId -> String -> String -> String
                   -> String -> AppHandler TaskInstance
createTaskInstance tid sid desc sol doc sig =
    Adapter.createTaskInstance $ TaskInstance 0 tid sid [] desc doc sol sig 


------------------------------------------------------------------------------
-- modification functions

-- TBI


------------------------------------------------------------------------------
-- delete functions

-- TBI
