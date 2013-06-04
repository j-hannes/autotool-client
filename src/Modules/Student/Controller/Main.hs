{-# LANGUAGE OverloadedStrings #-}

------------------------------------------------------------------------------
-- | Controller for the student page.
module Modules.Student.Controller.Main
    ( handleStudent
    , handleStudentSelection
    ) where

------------------------------------------------------------------------------
import           Data.Maybe                    (fromJust, isJust)
import qualified Data.Text                     as T
import           Data.Time                     (UTCTime, getCurrentTime)
------------------------------------------------------------------------------
import           Autotool.Client               as Autotool
import           Heist.Interpreted             (Splice)
import qualified Heist.Interpreted             as I
import           Snap                          (ifTop, liftIO)
import           Snap.Snaplet.Heist
------------------------------------------------------------------------------
import           Application                   (AppHandler)
import qualified Model.Base                    as Model
import qualified Model.Adapter.File.Assignment as Assignment
import qualified Model.Adapter.File.Course     as Course
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
    sid    <- getStudentId
    now    <- getCurrentTime
    groups <- Model.getGroupsCompleteByStudentId sid
    let splices = [
            ("studentId", I.textSplice . T.pack $ show sid)
          , ("groups",    I.mapSplices (renderGroups sid) groups) 
          ]
    heistLocal (I.bindSplices splices) $ render "student/index"


------------------------------------------------------------------------------
-- | Splice that is mapped over the <groups> tag to render group related
-- details into the template.
renderGroups :: (Group, Course, [(Assignment, Task, TaskInstance)]) -> UTCTime
             -> Splice AppHandler
renderGroups (group, course, assignments) now =
    I.runChildrenWith [
        ("groupDescription", groupNameSplice)
      , ("groupId",          groupIdSplice)
      , ("courseName",       courseNameSplice)
      , ("passCriteria",     passCriteriaSplice)
      , ("assignments",      assignmentsSplice)
      ]
  where
    groupNameSplice    = I.textSplice . T.pack        $ groupDescription group
    groupIdSplice      = I.textSplice . T.pack . show $ groupId group
    courseNameSplice   = I.textSplice . T.pack        $ courseName course
    passCriteriaSplice = I.textSplice . T.pack . show $ coursePassCriteria course
    assignmentsSplice  = I.mapSplices (renderAssignments now) assignments


------------------------------------------------------------------------------
-- | Splice that is mapped over the <assignments> tag to render assignment
-- related details into the template.
renderAssignments :: Integer -> Assignment -> Splice AppHandler
renderAssignments studentId assignment = do
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
