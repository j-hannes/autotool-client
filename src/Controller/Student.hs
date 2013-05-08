{-# LANGUAGE OverloadedStrings #-}

------------------------------------------------------------------------------
-- | Controller for the student page.
module Controller.Student
    ( handleStudent
    , handleStudentSelection
    ) where

------------------------------------------------------------------------------
import           Data.Maybe                    (fromJust, isJust)
import qualified Data.Text                     as T
import           Data.Time                     (getCurrentTime)
------------------------------------------------------------------------------
import           Autotool.Client               as Autotool
import           Heist.Interpreted             (Splice)
import qualified Heist.Interpreted             as I
import           Snap                          (ifTop, liftIO)
import           Snap.Snaplet.Heist
------------------------------------------------------------------------------
import           Application                   (AppHandler)
import qualified Model.Adapter.File.Assignment as Assignment
import qualified Model.Adapter.File.Course     as Course
import qualified Model.Adapter.File.Group      as Group
import qualified Model.Adapter.File.Task       as Task
import qualified Model.Adapter.File.TaskInstance as TaskInstance
import           Model.Types.Assignment
import           Model.Types.Course
import           Model.Types.Group
import           Model.Types.Task
import           Model.Types.TaskInstance
import           Utils.Auth                    (getStudentId)
import           Utils.Render                  (compareToNow, translateStatus)


handleStudentSelection :: AppHandler ()
handleStudentSelection = do
    render "student/pages/select"


------------------------------------------------------------------------------
-- | Renders the landing page with an overview of the tasks.
handleStudent :: AppHandler ()
handleStudent = ifTop $ do
    studentId <- getStudentId
    groups <- liftIO $ Group.getAllByStudentId studentId  -- uid
    let splices = [
            ("studentId", I.textSplice . T.pack $ show studentId)
          , ("groups", I.mapSplices (renderGroupRow studentId) groups) 
          ]
    heistLocal (I.bindSplices splices) $ render "student/index"


------------------------------------------------------------------------------
-- |
renderGroupRow :: Integer -> Group -> Splice AppHandler
renderGroupRow studentId group = do
    course      <- liftIO $ Course.getById (groupCourseId group)
    assignments <- liftIO $ Assignment.getAllByCourseId (courseId course)
    let splices = [
            ("groupDescription",
                I.textSplice . T.pack $ groupDescription group)
          , ("groupId",
                I.textSplice . T.pack . show $ groupId group)
          , ("courseName",
                I.textSplice . T.pack $ courseName course)
          , ("passCriteria",
                I.textSplice . T.pack . show $ coursePassCriteria course)
          , ("assignments",
                I.mapSplices (renderAssignmentRow studentId) assignments)
          ]
    I.runChildrenWith splices


renderAssignmentRow :: Integer -> Assignment -> Splice AppHandler
renderAssignmentRow studentId assignment = do
  task <- liftIO . Task.getById $ assignmentTaskId assignment
  taskInstance <- liftIO $ getCachedTaskInstance task studentId
  time <- liftIO getCurrentTime
  let splices = [
            ("description",
                I.textSplice . T.pack $ taskName task)
          , ("status",
                I.textSplice . translateStatus $ assignmentStatus assignment)
          , ("submissionTime",
                I.textSplice . T.pack $ compareToNow time
                                        (Just $ assignmentStart assignment)
                                        (Just $ assignmentEnd assignment))
          , ("taskInstanceId",
                I.textSplice . T.pack . show $ taskInstanceId taskInstance)
          ] 
  I.runChildrenWith splices

getCachedTaskInstance :: Task -> Integer -> IO TaskInstance
getCachedTaskInstance task userId = do
  loadedTaskInstance <- TaskInstance.getByTaskIdAndUserId (taskId task) userId
  if isJust loadedTaskInstance
    then return $ fromJust loadedTaskInstance
    else do
      (desc, sol, doc, sig) <- Autotool.getTaskInstance (taskSignature task)
                                                        (show userId)
      TaskInstance.create (taskId task) userId desc sol (show doc) sig
