{-# LANGUAGE OverloadedStrings #-}

------------------------------------------------------------------------------
-- | Controller for the tutor page.
module Controller.Tutor
    ( handleTutor
    ) where

------------------------------------------------------------------------------
-- import qualified Data.ByteString.Char8 as BS
import           Data.Maybe                    (fromJust, fromMaybe)
import qualified Data.Text                     as T
------------------------------------------------------------------------------
import           Heist.Interpreted             (Splice)
import qualified Heist.Interpreted             as I
import           Snap                          (ifTop, liftIO) --, writeBS)
import           Snap.Snaplet.Heist
------------------------------------------------------------------------------
import           Application                   (AppHandler)
import           Model.Adapter.File
import           Model.Types.Assignment        (Assignment)
import qualified Model.Types.Assignment        as Assignment
import           Model.Types.Course            (Course)
import qualified Model.Types.Course            as Course
import           Model.Types.TaskConfig        (TaskConfig)
import qualified Model.Types.TaskConfig        as TaskConfig


------------------------------------------------------------------------------
-- | Renders the landing page with an overview of the tasks.
handleTutor :: AppHandler ()
handleTutor = ifTop $ do
    courses     <- liftIO $ restoreAll "course"
    taskConfigs <- liftIO $ restoreAll "taskconfig"
    assignments <- liftIO $ restoreAll "assignment"
    let splices = [
            ("courses",     I.mapSplices renderCourseRow courses)
          , ("taskConfigs", I.mapSplices (renderTaskConfigRow assignments) taskConfigs)
          ]
    heistLocal (I.bindSplices splices) $ render "tutor/index"


------------------------------------------------------------------------------
-- |
renderCourseRow :: Course -> Splice AppHandler
renderCourseRow course =
    I.runChildrenWith splices
  where
    splices =
      [ ("courseId",        I.textSplice cid)
      , ("courseName",      I.textSplice cname)
      , ("courseSemester",  I.textSplice csemester)
      , ("enrollmentBegin", I.textSplice cenrbeg)
      , ("enrollmentEnd",   I.textSplice cenrend)
      , ("students",        I.textSplice cstudents)
      ]
    cid'      = fromJust $ Course.cid course
    cid       = T.pack . show $ cid'
    cname     = T.pack                          $ Course.courseName      course
    csemester = T.pack                          $ Course.semester        course
    cenrbeg   = T.pack . fromMaybe "n/a" $ fmap show $ Course.enrollmentBegin course
    cenrend   = T.pack . fromMaybe "n/a" $ fmap show $ Course.enrollmentEnd   course
    cstudents = T.pack . show $ 42


------------------------------------------------------------------------------
-- |
renderTaskConfigRow :: [Assignment] -> TaskConfig -> Splice AppHandler
renderTaskConfigRow assn taskConfig =
    I.runChildrenWith splices
  where
    splices =
      [ ("taskConfigId",   I.textSplice tcid)
      , ("taskConfigName", I.textSplice tcname)
      , ("taskType",       I.textSplice ttype)
      , ("dateCreated",    I.textSplice tcreated)
      , ("assignments",    I.textSplice tassignments)
      ]
    tcid'        = fromJust $ TaskConfig.tcid taskConfig
    tcid         = T.pack . show $ tcid'
    tcname       = T.pack                   $ TaskConfig.title       taskConfig
    ttype        = T.pack                   $ TaskConfig.name        taskConfig
    tcreated     = T.pack . show            $ TaskConfig.created     taskConfig
    tassignments = T.pack . show $ length $ filter ((== tcid') . Assignment.taskId) assn
