{-# LANGUAGE OverloadedStrings #-}

------------------------------------------------------------------------------
-- | Controller to display the task list.
module Controller.Tasks
    ( showTaskList
    ) where

------------------------------------------------------------------------------
import qualified Data.Text                     as T
------------------------------------------------------------------------------
import           Heist.Interpreted             (Splice)
import qualified Heist.Interpreted             as I
import           Snap                          (ifTop, liftIO)
import           Snap.Snaplet.Heist
------------------------------------------------------------------------------
import           Application                   (AppHandler)
import qualified Model.Adapter.File.Assignment as Assignment
import qualified Model.Adapter.File.Task       as Task
import           Model.Types.Task


------------------------------------------------------------------------------
-- | Renders the landing page with an overview of the tasks.
showTaskList :: AppHandler ()
showTaskList = ifTop $ do
    tasks   <- liftIO $ Task.getAllByTutorId 1
    let splices = [("tasks", I.mapSplices renderTaskRow   tasks)]
    heistLocal (I.bindSplices splices) $ render "tutor/pages/tasks"


------------------------------------------------------------------------------
-- |
renderTaskRow :: Task -> Splice AppHandler
renderTaskRow task = do
    assignmentCount <- liftIO $ Assignment.countByTaskId (taskId task)
    let tassignments = T.pack $ show assignmentCount
        splices =
          [ ("taskConfigId",   I.textSplice tid)
          , ("taskConfigName", I.textSplice tname)
          , ("taskType",       I.textSplice ttype)
          , ("dateCreated",    I.textSplice tcreated)
          , ("assignments",    I.textSplice tassignments)
          ]
    I.runChildrenWith splices
  where
    tid          = T.pack . show $ taskId      task
    tname        = T.pack        $ taskName    task
    ttype        = T.pack        $ taskType    task
    tcreated     = T.pack . show $ taskCreated task
