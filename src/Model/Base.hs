module Model.Base where

import Control.Monad                      (forM)
import Data.Maybe                         (fromJust, isJust)
import Data.Time                          (UTCTime)
import Snap                               (liftIO)

import Application                        (AppHandler)
import Autotool.Client                    as Autotool
import Autotool.Client.Types.ScoringOrder
import Model.Adapter.IORef                as Adapter
import Model.Types.Assignment
import Model.Types.Course
import Model.Types.Enrollment
import Model.Types.Group
import Model.Types.Solution
import Model.Types.Task
import Model.Types.TaskInstance


-- consider moving to types

type CourseBundle     = (Course, [(Assignment, Task)])
type GroupBundle      = (Group, Course, [AssignmentBundle])
type AssignmentBundle = (Assignment, Task, TaskInstance)


-- Simple accessor functions

-- get

getAssignmentsByCourseId :: Integer -> AppHandler [Assignment]
getAssignmentsByCourseId cid = do
  assignments <- Adapter.getAssignments
  return $ filter (\a -> assignmentCourseId a == cid) assignments

getAssignmentsForCourse :: Integer -> AppHandler [Assignment]
getAssignmentsForCourse cid = do
  assignments <- Adapter.getAssignments
  return $ filter (\a -> assignmentCourseId a == cid) assignments

getCourse :: Integer -> AppHandler Course
getCourse cid = do
  courses <- Adapter.getCourses
  return $ head $ filter (\c -> courseId c == cid) courses

getCourses :: [Integer] -> AppHandler [Course]
getCourses cids = do
  courses <- Adapter.getCourses
  return $ filter (\c -> courseId c `elem` cids) courses

getCoursesExcept :: [Integer] -> AppHandler [Course]
getCoursesExcept cids = do
  courses <- Adapter.getCourses
  return $ filter (\c -> not $ courseId c `elem` cids) courses

getGroups :: [Integer] -> AppHandler [Group]
getGroups cids = do
  groups <- Adapter.getGroups
  return $ filter (\c -> groupId c `elem` cids) groups

getCoursesByTutorId :: Integer -> AppHandler [Course]
getCoursesByTutorId tid = do
  courses <- Adapter.getCourses
  return $ filter (\c -> courseTutorId c == tid) courses

getSolutionsByTaskInstanceId :: Integer -> AppHandler [Solution]
getSolutionsByTaskInstanceId tid = do
  solutions <- Adapter.getSolutions
  return $ filter (\c -> solutionTaskInstanceId c == tid) solutions

getTaskInstanceById :: Integer -> AppHandler TaskInstance
getTaskInstanceById tiid = do
  taskInstances <- Adapter.getTaskInstances
  return $ head $ filter (\t -> taskInstanceId t == tiid) taskInstances

getTaskInstanceForTask :: Integer -> Integer -> AppHandler (Maybe TaskInstance)
getTaskInstanceForTask tid sid = do
  taskInstances <- Adapter.getTaskInstances
  let ti' = filter (\ti -> taskInstanceTaskId ti == tid &&
                           taskInstanceStudentId ti == sid) taskInstances
  if null ti' then return Nothing else return $ Just $ head ti'

getTask :: Integer -> AppHandler Task
getTask tid = do
  tasks <- Adapter.getTasks
  return $ head $ filter (\t -> taskId t == tid) tasks

getTasks :: [Integer] -> AppHandler [Task]
getTasks tids = do
  tasks <- Adapter.getTasks
  return $ filter (\t -> taskId t `elem` tids) tasks

getTasksByTutorId :: Integer -> AppHandler [Task]
getTasksByTutorId tid = do
  tasks <- Adapter.getTasks
  return $ filter (\t -> taskTutorId t == tid) tasks


-- create

createAssignment :: Integer -> Integer -> Status -> UTCTime -> UTCTime
                 -> AppHandler Assignment
createAssignment cid tid sts start end =
    Adapter.createAssignment $ Assignment 0 cid tid sts start end

createCourse :: Integer -> String -> String -> Maybe UTCTime -> Maybe UTCTime
             -> Double -> AppHandler Course
createCourse tid name sem enrStart enrEnd pass =
    Adapter.createCourse $ Course 0 tid name sem enrStart enrEnd pass

createEnrollment :: Integer -> Integer -> UTCTime -> AppHandler Enrollment
createEnrollment gid sid time =
    Adapter.createEnrollment $ Enrollment 0 gid sid time

createGroup :: Integer -> String -> Int -> AppHandler Group
createGroup cid desc cap =
    Adapter.createGroup $ Group 0 cid desc cap

createSolution :: Integer -> String -> String -> Maybe Result -> UTCTime
               -> AppHandler Solution
createSolution tid cont eval res time =
    Adapter.createSolution $ Solution 0 tid cont eval res time

createTask :: Integer -> String -> String -> String -> ScoringOrder -> UTCTime
           -> AppHandler Task
createTask tid name ttpe sig so time =
    Adapter.createTask $ Task 0 tid name ttpe sig so time

createTaskInstance :: Integer -> Integer -> String -> String -> String
                   -> String -> AppHandler TaskInstance
createTaskInstance tid uid desc sol doc sig =
    Adapter.createTaskInstance $ TaskInstance 0 tid uid desc doc sol sig 

-- Complex accessor functions

getCoursesByStudentId :: Integer -> AppHandler [Course]
getCoursesByStudentId sid = do
    groups <- Model.Base.getGroupsByStudentId sid
    Model.Base.getCourses $ map groupCourseId groups

getGroupBundlesByStudentId :: Integer -> AppHandler [GroupBundle]
getGroupBundlesByStudentId sid = do
    groups <- Model.Base.getGroupsByStudentId sid
    forM groups filterGroups
  where
    filterGroups group = do
      course      <- Model.Base.getCourse (groupCourseId group) 
      assignments <- Model.Base.getAssignmentsForCourse (courseId course) 
      bundle      <- forM assignments filterAssignments
      return (group, course, bundle)
    filterAssignments assignment = do
      task         <- Model.Base.getTask (assignmentTaskId assignment)
      taskInstance <- Model.Base.getCachedTaskInstance task sid
      return (assignment, task, taskInstance)

getCourseBundlesByTutorId :: Integer -> AppHandler [CourseBundle]
getCourseBundlesByTutorId tid = do
    courses <- Model.Base.getCoursesByTutorId tid
    forM courses filterCourses
  where
    filterCourses course = do
      assignments <- Model.Base.getAssignmentsByCourseId (courseId course)
      bundle      <- forM assignments filterAssignments
      return (course, bundle)
    filterAssignments assignment = do
      task <- getTask (assignmentTaskId assignment)
      return (assignment, task)

getGroupsByStudentId :: Integer -> AppHandler [Group]
getGroupsByStudentId sid = do
    enrollments <- Adapter.getEnrollments
    let e' = filter (\e -> enrollmentStudentId e == sid) enrollments
    Model.Base.getGroups $ map enrollmentGroupId e'

getTasksWithAssignmentCount :: Integer -> AppHandler [(Task, Int)]
getTasksWithAssignmentCount tid = do
    tasks       <- Adapter.getTasks
    let t' = filter (\t -> taskTutorId t == tid) tasks
    assignments <- Adapter.getAssignments
    forM t' (filterTasks assignments)
  where
    filterTasks assignments task = do
      let a' = filter (\a -> assignmentTaskId a == taskId task) assignments
      return (task, length a')

getCoursesWithEnrollableGroups :: Integer -> AppHandler [(Course, [Group])]
getCoursesWithEnrollableGroups sid = do
    courses  <- Model.Base.getCoursesByStudentId sid
    courses' <- Model.Base.getCoursesExcept (map courseId courses)
    groups  <- Adapter.getGroups
    forM courses' (filterCourses groups)
  where
    filterCourses groups course = do 
      let g' = filter (\g -> groupCourseId g == courseId course) groups
      return (course, g')

getCachedTaskInstance :: Task -> Integer -> AppHandler TaskInstance
getCachedTaskInstance task sid = do
  loadedTaskInstance <- Model.Base.getTaskInstanceForTask (taskId task) sid
  if isJust loadedTaskInstance
    then return $ fromJust loadedTaskInstance
    else do
      (desc, sol, doc, sig) <- liftIO $ Autotool.getTaskInstance
                                          (taskSignature task) (show sid)
      Model.Base.createTaskInstance (taskId task) sid desc sol (show doc) sig
