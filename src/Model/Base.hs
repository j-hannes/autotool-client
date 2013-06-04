module Model.Base where

import Data.Time              (UTCTime)

import Application            (AppHandler)
import Autotool.Client.Types.ScoringOrder
import Model.Adapter.IORef    as Adapter
import Model.Types.Assignment
import Model.Types.Course
import Model.Types.Enrollment
import Model.Types.Group
import Model.Types.Solution
import Model.Types.Task
import Model.Types.TaskInstance


-- Simple accessor functions

-- get

getAssignmentsByCourseIds :: [Integer] -> AppHandler [Assignment]
getAssignmentsByCourseIds cids = do
  assignments <- Adapter.getAssignments
  return $ filter (\a -> assignmentCourseId a `elem` cids) assignments

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


-- Complex accessor functions

getCoursesByStudentId :: Integer -> AppHandler [Course]
getCoursesByStudentId sid = do
    courses     <- Adapter.getCourses
    groups      <- Adapter.getGroups
    enrollments <- Adapter.getEnrollments
    return $ filter (f groups enrollments) courses
  where
    f g e c = courseId c `elem` (map groupCourseId $ filter (h e) g)
    h e g   = groupId  g `elem` (map enrollmentGroupId $ filter i e)
    i e     = enrollmentStudentId e == sid
    

getGroupsCompleteByStudentId :: Integer
                             -> AppHandler 
                                  [(Group, Course,
                                    [(Assignment, Task, TaskInstance)])]
getGroupsCompleteByStudentId sid = do
    courses     <- Model.Base.getCoursesByStudentId sid
    assignments <- Model.Base.getAssignmentsByCourseIds (map courseId courses)
    tasks       <- Model.Base.getTasks (map assignmentTaskId assignments)
    groups      <- Adapter.Model.Base.getCoursesByStudentId sid
    return $ map (f tasks assignments) courses
  where 
    f t a c = (c, map (g t) (filter (h c) a))
    g t a   = (a, filter (i a) t)
    h c a   = assignmentCourseId a == courseId c
    i a t   = assignmentTaskId   a == taskId   t

getCoursesCompleteByTutorId :: Integer -- FIXME
                            -> AppHandler [(Course, [(Assignment, [Task])])]
getCoursesCompleteByTutorId tid = do
    courses     <- Model.Base.getCoursesByTutorId tid
    assignments <- Model.Base.getAssignmentsByCourseIds (map courseId courses)
    tasks       <- Model.Base.getTasks (map assignmentTaskId assignments)
    return $ map (f tasks assignments) courses
  where 
    f t a c = (c, map (g t) (filter (h c) a))
    g t a   = (a, filter (i a) t)
    h c a   = assignmentCourseId a == courseId c
    i a t   = assignmentTaskId   a == taskId   t

getGroupsByStudentId :: Integer -> AppHandler [Group]
getGroupsByStudentId sid = do
    groups      <- Adapter.getGroups
    enrollments <- Adapter.getEnrollments
    return $ filter (f (filter h enrollments)) groups
  where
    f e g = groupId g `elem` map enrollmentGroupId e
    h e   = enrollmentStudentId e == sid

getTasksWithAssignmentCount :: Integer -> AppHandler [(Task, Int)]
getTasksWithAssignmentCount tid = do
    tasks       <- Adapter.getTasks
    assignments <- Adapter.getAssignments
    return $ map (f assignments) (h tasks)
  where
    f a t = (t, length $ filter (g t) a)
    g t a = assignmentTaskId a == taskId t
    h t   = filter i t
    i t   = taskId t == tid

------------------------------------------------------------------------------
-- | TODO: I belive this can be refactored a lot ...
getCoursesWithGroups :: Integer -> AppHandler [(Course, [Group])]
getCoursesWithGroups sid = do
    courses <- Adapter.getCourses 
    groups  <- getEnrollableGroups sid
    let courses' = filter (f groups) courses
    return $ map (h groups) courses'
  where 
    f g c = courseId c `elem` (map groupCourseId g)
    h g c = (c, filter (i c) g)
    i c g = courseId c == groupCourseId g

getEnrollableGroups :: Integer -> AppHandler [Group]
getEnrollableGroups uid = do
    enrollments <- Adapter.getEnrollments
    groups      <- Adapter.getGroups
    return $ filterEnrollableGroups uid enrollments groups

filterEnrollableGroups :: Integer -> [Enrollment] -> [Group] -> [Group]
filterEnrollableGroups uid enrollments groups =
    filter (not . belongsToEnrolledCourse) groups
  where
    belongsToEnrolledCourse group = groupCourseId group `elem` enrolledCourseIds
    enrolledCourseIds  = map groupCourseId enrolledGroups
    enrolledGroups     = filter isEnrolled groups
    isEnrolled group   = groupId group `elem` enrolledGroupIds
    belongsToStudent e = enrollmentStudentId e == uid

    enrolledGroupIds   = map enrollmentGroupId studentEnrollments
    studentEnrollments = filter belongsToStudent  enrollments
