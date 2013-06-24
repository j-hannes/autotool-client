{-# LANGUAGE OverloadedStrings #-}

------------------------------------------------------------------------------
-- | Controller to display the task list.
module Modules.Tutor.Controller.Tasks
    ( showTaskList
    ) where

------------------------------------------------------------------------------
import           Data.Maybe             (fromJust)
import qualified Data.Text              as T
------------------------------------------------------------------------------
import           Heist.Interpreted      (Splice)
import qualified Heist.Interpreted      as I
import           Snap                   (ifTop, (<$>))
import           Snap.Snaplet.Heist
------------------------------------------------------------------------------
import           Application            (AppHandler)
import qualified Database.Switch        as Database
import           Model.Datatypes


------------------------------------------------------------------------------
-- | Renders the task page with a list of all tasks of the logged in tutor.
showTaskList :: AppHandler ()
showTaskList = ifTop $ do
    tutor <- fromJust <$> Database.getTutor "51c8235ef4d13fc80f76c462"
    tasks <- Database.getTasksWithAssignmentCount tutor
    let splices = [("tasks", I.mapSplices renderTasks tasks)]
    heistLocal (I.bindSplices splices) $ render "tutor/pages/tasks"


------------------------------------------------------------------------------
-- | Splice that is mapped over the <tasks> tag to render task related details
-- into the template.
renderTasks :: (Task, Int) -> Splice AppHandler
renderTasks (task, assignments) = do
    I.runChildrenWith [
            ("taskConfigId",   taskIdSplice)
          , ("taskConfigName", taskNameSplice)
          , ("taskType",       taskTypeSplice)
          , ("dateCreated",    dateCreatedSplice)
          , ("assignments",    assignmentsSplice)
          ]
  where
    taskIdSplice      = I.textSplice . T.pack . show $ taskId      task
    taskNameSplice    = I.textSplice . T.pack        $ taskName    task
    taskTypeSplice    = I.textSplice . T.pack        $ taskType    task
    dateCreatedSplice = I.textSplice . T.pack . show $ taskCreated task
    assignmentsSplice = I.textSplice . T.pack . show $ assignments
