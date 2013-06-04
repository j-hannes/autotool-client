module Model.Adapter.IORef where

import Data.IORef          (readIORef, writeIORef)
import Snap                (gets, liftIO)

import Application         (App(..), AppHandler)
import Model.Indexable
import Model.Types.Assignment
import Model.Types.Course
import Model.Types.Enrollment
import Model.Types.Group
import Model.Types.Solution
import Model.Types.Task
import Model.Types.TaskInstance

-- get

getAssignments :: AppHandler [Assignment]
getAssignments = do
    assignmentRef <- gets _assignments
    liftIO $ readIORef assignmentRef

getCourses :: AppHandler [Course]
getCourses = do
    courseRef <- gets _courses
    liftIO $ readIORef courseRef

getEnrollments :: AppHandler [Enrollment]
getEnrollments = do
    enrollmentRef <- gets _enrollments
    liftIO $ readIORef enrollmentRef

getGroups :: AppHandler [Group]
getGroups = do
    groupRef <- gets _groups
    liftIO $ readIORef groupRef

getSolutions :: AppHandler [Solution]
getSolutions = do
    solutionRef <- gets _solutions
    liftIO $ readIORef solutionRef

getTasks :: AppHandler [Task]
getTasks = do
    taskRef <- gets _tasks
    liftIO $ readIORef taskRef

getTaskInstances :: AppHandler [TaskInstance]
getTaskInstances = do
    taskInstanceRef <- gets _taskInstances
    liftIO $ readIORef taskInstanceRef

-- create

createAssignment :: Assignment -> AppHandler Assignment
createAssignment assignment = do
    assignmentRef <- gets _assignments
    assignments <- liftIO $ readIORef assignmentRef
    let nextId    = getNextId 0 assignments
        newAssignment  = setId assignment nextId
        newAssignments = newAssignment : assignments
    liftIO $ writeIORef assignmentRef newAssignments
    return $ newAssignment

createCourse :: Course -> AppHandler Course
createCourse course = do
    courseRef <- gets _courses
    courses <- liftIO $ readIORef courseRef
    let nextId     = getNextId 0 courses
        newCourse  = setId course nextId
        newCourses = newCourse : courses
    liftIO $ writeIORef courseRef newCourses
    return $ newCourse

createEnrollment :: Enrollment -> AppHandler Enrollment
createEnrollment enrollment = do
    enrollmentRef <- gets _enrollments
    enrollments <- liftIO $ readIORef enrollmentRef
    let nextId     = getNextId 0 enrollments
        newEnrollment  = setId enrollment nextId
        newEnrollments = newEnrollment : enrollments
    liftIO $ writeIORef enrollmentRef newEnrollments
    return $ newEnrollment

createGroup :: Group -> AppHandler Group
createGroup group = do
    groupRef <- gets _groups
    groups <- liftIO $ readIORef groupRef
    let nextId    = getNextId 0 groups
        newGroup  = setId group nextId
        newGroups = newGroup : groups
    liftIO $ writeIORef groupRef newGroups
    return $ newGroup

createSolution :: Solution -> AppHandler Solution
createSolution solution = do
    solutionRef <- gets _solutions
    solutions <- liftIO $ readIORef solutionRef
    let nextId    = getNextId 0 solutions
        newSolution  = setId solution nextId
        newSolutions = newSolution : solutions
    liftIO $ writeIORef solutionRef newSolutions
    return $ newSolution

createTask :: Task -> AppHandler Task
createTask task = do
    taskRef <- gets _tasks
    tasks <- liftIO $ readIORef taskRef
    let nextId    = getNextId 0 tasks
        newTask  = setId task nextId
        newTasks = newTask : tasks
    liftIO $ writeIORef taskRef newTasks
    return $ newTask

createTaskInstance :: TaskInstance -> AppHandler TaskInstance
createTaskInstance taskInstance = do
    taskInstanceRef <- gets _taskInstances
    taskInstances <- liftIO $ readIORef taskInstanceRef
    let nextId    = getNextId 0 taskInstances
        newTaskInstance  = setId taskInstance nextId
        newTaskInstances = newTaskInstance : taskInstances
    liftIO $ writeIORef taskInstanceRef newTaskInstances
    return $ newTaskInstance

------------------------------------------------------------------------------
-- | Return the highest found index (id) + 1 from a list of indexable data
-- types.
getNextId :: (Indexable a) => Integer -> [a] -> Integer
getNextId n [] = n + 1
getNextId n (x:xs) | i > n     = getNextId i xs
                   | otherwise = getNextId n xs        
                   where
                     i = iid x
