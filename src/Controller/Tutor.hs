{-# LANGUAGE OverloadedStrings #-}

------------------------------------------------------------------------------
-- | Controller for the tutor page.
module Controller.Tutor
    ( handleTutor
    ) where

------------------------------------------------------------------------------
import qualified Data.Text                     as T
import           Data.Time                     (UTCTime, getCurrentTime)
------------------------------------------------------------------------------
import           Heist.Interpreted             (Splice)
import qualified Heist.Interpreted             as I
import           Snap                          (ifTop, liftIO)
import           Snap.Snaplet.Heist
------------------------------------------------------------------------------
import           Application                   (AppHandler)
import qualified Model.Adapter.File.Assignment as Assignment
import qualified Model.Adapter.File.Course     as Course
import qualified Model.Adapter.File.Task       as Task
import           Model.Types.Assignment
import           Model.Types.Course
import           Model.Types.Task
import           Utils.Render                  (compareToNow, translateStatus)


------------------------------------------------------------------------------
-- | Renders the landing page with an overview of the tasks.
handleTutor :: AppHandler ()
handleTutor = ifTop $ do
    courses <- liftIO $ Course.getAllByTutorId 1
    let splices = [
            ("courses", I.mapSplices renderCourseRow courses)
          ]
    heistLocal (I.bindSplices splices) $ render "tutor/index"


------------------------------------------------------------------------------
-- |
renderCourseRow :: Course -> Splice AppHandler
renderCourseRow course = do
    assignments <- liftIO $ Assignment.getAllByCourseId (courseId course)
    taskConfigs <- liftIO $ Task.getAllByTutorId 1
    let courseAssignments = filter (isAssignmentForCourse course) assignments
        splices enrollment time =
          [ ("courseId",        I.textSplice cid)
          , ("courseName",      I.textSplice cname)
          , ("enrollment",      I.textSplice $ T.pack enrollment)
          , ("students",        I.textSplice cstudents)
          , ("passCriteria",    I.textSplice cpass)
          , ("passRate",        I.textSplice "??")
          , ("assignedtasks",   I.mapSplices
                                  (renderAssignmentRow time taskConfigs)
                                  courseAssignments)
          ]
    time <- liftIO getCurrentTime
    let enrollment = compareToNow time (courseEnrollmentFrom course)
                                       (courseEnrollmentTo course)
    I.runChildrenWith (splices enrollment time)
  where
    cid'        = courseId course
    cid         = T.pack . show $ cid'
    cname       = T.pack        $ courseName course
    cstudents   = T.pack . show $ (23 :: Int)
    cpass       = T.pack . show $ doubleToInt $ coursePassCriteria course

doubleToInt :: Double -> Int
doubleToInt = read . takeWhile ((/=) '.') . show

------------------------------------------------------------------------------
-- |
isAssignmentForCourse :: Course -> Assignment -> Bool
isAssignmentForCourse course = (== (courseId course)) . assignmentCourseId


------------------------------------------------------------------------------
-- |
renderAssignmentRow :: UTCTime -> [Task] -> Assignment
                    -> Splice AppHandler
renderAssignmentRow now taskConfigs assignment = do
    let taskConfig = filter (isTaskForAssignment assignment) taskConfigs
    I.runChildrenWithText (splices $ head taskConfig)
  where
    splices taskConfig = [
        ("taskname", T.pack $ taskName taskConfig)
      , ("tasktype", T.pack $ taskType taskConfig)
      , ("status",   translateStatus $ assignmentStatus assignment)
      , ("timespan", T.pack $ iTime)
      ]
    iTime = compareToNow now
                         (Just $ assignmentStart assignment)
                         (Just $ assignmentEnd assignment)


------------------------------------------------------------------------------
-- |
isTaskForAssignment :: Assignment -> Task -> Bool
isTaskForAssignment assignment = 
    (== (assignmentId assignment)) . taskId
