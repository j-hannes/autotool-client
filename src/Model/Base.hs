module Model.Base (
    -- ^ retrieve one by pk
    Db.getCourse
  , Db.getStudent
  , Db.getTask
  , Db.getTaskInstance
  , Db.getTutor

    -- ^ retrieve all
  , Db.getGroups

    -- ^ retrieve many by index 
  , Db.getAssignmentsByCourse
  , Db.getCoursesByTutor
  , Db.getGroupsByCourse
  , Db.getSolutionsByTaskInstance
  , Db.getTasksByTutor

    -- ^ create
  , Db.createAssignment
  , Db.createCourse
  , Db.createEnrollment
  , Db.createGroup
  , Db.createSolution
  , Db.createTask

    -- ^ joined retrieve
  , getEnrollableCourses
  , getEnrolledGroups
  , getTasksWithAssignmentCount
  , getCachedTaskInstance    

    -- ^ other
  , Db.getLastSolutionsByTaskInstance

  ) where

------------------------------------------------------------------------------
import Control.Monad           (forM)
import Data.Maybe              (fromJust)
import Snap                    (liftIO, (<$>))
------------------------------------------------------------------------------
import Application             (AppHandler)
import Autotool.Client         as Autotool
------------------------------------------------------------------------------
import Model.Adapter.Sqlite    as Db
import Model.Types


------------------------------------------------------------------------------

getEnrollableCourses :: Student -> AppHandler [Course]
getEnrollableCourses student = do
    groups      <- Model.Base.getEnrolledGroups student
    courses     <- Db.getAllCourses
    return $ filter (\c -> not $ courseId c `elem` (map groupCourseId groups))
               courses

getEnrolledGroups :: Student -> AppHandler [Group]
getEnrolledGroups student = do
    enrollments <- Db.getEnrollmentsByStudent (studentId student)
    Db.getGroups (map enrollmentGroupId enrollments)

------------------------------------------------------------------------------

getTasksWithAssignmentCount :: Tutor -> AppHandler [(Task, Int)]
getTasksWithAssignmentCount tutor = do
    tasks <- Db.getTasksByTutor (tutorId tutor)
    forM tasks filterTasks
  where
    filterTasks task = do
      assignments <- Db.getAssignmentsByTask (taskId task)
      return (task, length assignments)

------------------------------------------------------------------------------

getCachedTaskInstance :: Assignment -> Student -> AppHandler TaskInstance
getCachedTaskInstance assignment student = do
    taskInstances <- Db.getTaskInstancesByAssignment
                       (assignmentId assignment)
    let taskInstances' = filterInstances taskInstances
    if null taskInstances'
      then do
        task <- fromJust <$> Db.getTask (assignmentTaskId assignment)
        (desc, sol, doc, sig) <- liftIO $ Autotool.getTaskInstance
                                            (taskSignature task) (show sid)
        tid <- Db.createTaskInstance
                 (assignmentId assignment, sid, desc, show doc, sol, sig)
        fromJust <$> Db.getTaskInstance tid
      else
        return $ head taskInstances'
  where
    sid             = studentId student
    filterInstances = filter (\ti -> taskInstanceStudentId ti == sid)
