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
import           Data.Time                     (getCurrentTime)
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
            ("courses",     I.mapSplices
                              (renderCourseRow assignments) 
                              courses)
          , ("taskConfigs", I.mapSplices
                              (renderTaskConfigRow assignments)
                              taskConfigs)
          ]
    heistLocal (I.bindSplices splices) $ render "tutor/index"


------------------------------------------------------------------------------
-- |
renderCourseRow :: Course -> [Assignment] -> Splice AppHandler
renderCourseRow course assignments = do
    time <- liftIO getCurrentTime
    let enrollment = compareToNow time (Course.enrollmentBegin course)
                                       (Course.enrollmentEnd course)
    I.runChildrenWith (splices enrollment)
  where
    splices enrollment =
      [ ("courseId",        I.textSplice cid)
      , ("courseName",      I.textSplice cname)
      , ("enrollment",      I.textSplice $ T.pack enrollment)
      , ("students",        I.textSplice cstudents)
      , ("capacity",        I.textSplice ccapacity)
      , ("assignedtasks",   I.mapSplices
                              (renderAssignentRow $ filter (Assignment.courseId . ((==) Course.cid course) assignments))
      ]
    cid'        = fromJust $ Course.cid course
    cid         = T.pack . show $ cid'
    cname       = T.pack        $ Course.courseName course
    cstudents   = T.pack . show $ 23
    ccapacity   = T.pack . show $ Course.capacity course

renderAssignmentRow assignment = do
    taskConfigs <- liftIO
    I.runChildrenWith splices
  where
    splices = [
      ("taskname", I.TextSplice )
    ]

compareToNow _   Nothing      _          = "offen"
compareToNow _   _            Nothing    = "offen"
compareToNow now (Just begin) (Just end)
    | now < begin = (show begin) ++ " - " ++ (show end)
    | now < end = "noch bis " ++ (show end)
    | otherwise = "vorbei seit " ++ (show end)
    

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
