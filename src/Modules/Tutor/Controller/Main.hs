{-# LANGUAGE OverloadedStrings #-}

------------------------------------------------------------------------------
-- | Controller for the tutor page.
module Modules.Tutor.Controller.Main
    ( handleTutor
    ) where

------------------------------------------------------------------------------
import qualified Data.Text              as T
import           Data.Time              (UTCTime, getCurrentTime)
------------------------------------------------------------------------------
import           Heist.Interpreted      (Splice)
import qualified Heist.Interpreted      as I
import           Snap                   (ifTop, liftIO)
import           Snap.Snaplet.Heist
------------------------------------------------------------------------------
import           Application            (AppHandler)
import qualified Model.Base             as Model
import           Model.Types.Assignment
import           Model.Types.Course
import           Model.Types.Task
import           Utils.Render           (compareToNow, doubleToInt)
import           Utils.Render           (translateStatus)


------------------------------------------------------------------------------
-- | Renders the landing page with an overview over courses and assignments.
handleTutor :: AppHandler ()
handleTutor = ifTop $ do
    courses <- Model.getCoursesCompleteByTutorId 1
    now <- liftIO getCurrentTime
    let splice = I.mapSplices (renderCourses now) courses
    heistLocal (I.bindSplice "courses" splice) $ render "tutor/index"


------------------------------------------------------------------------------
-- | Splice that is mapped over the <courses> tag to render course related
-- details into the template.
renderCourses :: UTCTime -> (Course, [(Assignment, [Task])])
              -> Splice AppHandler
renderCourses now (course, assignments) = do
    liftIO $ print assignments
    I.runChildrenWith [
        ("courseId",      courseIdSplice)
      , ("courseName",    courseNameSplice)
      , ("enrollment",    enrollmentSplice)
      , ("students",      studentSplice)
      , ("passCriteria",  passCriteriaSplice)
      , ("passRate",      passRateSplice)
      , ("assignedTasks", assignedTasksSplice)
      ]
  where
    courseIdSplice      = I.textSplice . T.pack . show $ courseId   course
    courseNameSplice    = I.textSplice . T.pack        $ courseName course
    enrollmentSplice    = I.textSplice . T.pack        $ enrollmentText
    studentSplice       = I.textSplice . T.pack . show $ students
    passCriteriaSplice  = I.textSplice . T.pack . show $ passCriteria
    passRateSplice      = I.textSplice . T.pack        $ passRate
    assignedTasksSplice = I.mapSplices (renderAssignments now) assignments

    passRate       = "??"       -- FIXME
    students       = 23 :: Int  -- FIXME
    passCriteria   = doubleToInt $ coursePassCriteria course
    enrollmentText = compareToNow now (courseEnrollmentFrom course)
                                      (courseEnrollmentTo course)


------------------------------------------------------------------------------
-- | Splice that is mapped over the <assignedTasks> tag to render assignment
-- specific details into the template.
--
-- TODO: check if things here are done alright (head?)
renderAssignments :: UTCTime -> (Assignment, [Task]) -> Splice AppHandler
renderAssignments now (assignment, tasks) = do
    I.runChildrenWithText (splices $ head tasks)
  where
    splices taskConfig = [
        ("taskName", T.pack $ taskName taskConfig)
      , ("taskType", T.pack $ taskType taskConfig)
      , ("status",   translateStatus $ assignmentStatus assignment)
      , ("timespan", T.pack $ iTime)
      ]
    iTime = compareToNow now
                         (Just $ assignmentStart assignment)
                         (Just $ assignmentEnd assignment)
