{-# LANGUAGE OverloadedStrings #-}

------------------------------------------------------------------------------
-- | Controller for creating tasks.
module Controller.Task
    ( handleTaskTree
    ) where

------------------------------------------------------------------------------
import qualified Data.Text as T
import           Snap
import           Snap.Snaplet.Heist
------------------------------------------------------------------------------
import           Application
import           Autotool.Client.Interface.Direct
import qualified View.Task as View


------------------------------------------------------------------------------
-- | Renders a tree (list) of all available tasks of the autotool.
handleTaskTree :: AppHandler ()
handleTaskTree = do
    taskTrees <- liftIO getTaskTypes
    heistLocal
      (View.bindTaskTreeSplice taskTrees)
      (render View.taskTreeTemplate)
