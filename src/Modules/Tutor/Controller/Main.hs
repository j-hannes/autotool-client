{-# LANGUAGE OverloadedStrings #-}

------------------------------------------------------------------------------
-- | Controller for the tutor page.
module Modules.Tutor.Controller.Main
    ( handleTutor
    ) where

------------------------------------------------------------------------------
import           Data.Maybe         (fromJust)
import           Data.Time          (getCurrentTime)
------------------------------------------------------------------------------
import           Heist.Interpreted  (Splice)
import qualified Heist.Interpreted  as I
import           Snap               (ifTop, lift, liftIO, (<$>), redirect)
import           Snap.Snaplet.Heist
------------------------------------------------------------------------------
import           Application        (AppHandler)
import qualified Database.Switch    as Database
import           Model.Datatypes
import           Utils.Render


------------------------------------------------------------------------------
-- | Renders the landing page with an overview over courses and assignments.
handleTutor :: AppHandler ()
handleTutor = ifTop $ do
    mTutor <- Database.getTutor "51c8235ef4d13fc80f76c462"
    case mTutor of
      Nothing -> redirect "/404"
      Just tutor -> continueWith tutor
  where
    continueWith tutor = do
      courses <- Database.getCoursesByTutor (tutorId tutor)
      let splice = I.mapSplices renderCourse courses
      heistLocal (I.bindSplice "courses" splice) $ render "tutor/index"


------------------------------------------------------------------------------
-- | Splice that is mapped over the <courses> tag to render course related
-- details into the template.
renderCourse :: Course -> Splice AppHandler
renderCourse course = do
    now         <- liftIO getCurrentTime
    assignments <- lift $ Database.getAssignmentsByCourse (courseId course)
    I.runChildrenWith [
        ("courseId",      courseId           |< course)
      , ("passCriteria",  coursePassCriteria |< course)
      , ("courseName",    courseName         |- course)
      , ("enrollment",    id                 |- compareToNow now from to)
      , ("assignedTasks", I.mapSplices renderAssignment assignments)
      ]
  where
    from  = courseEnrollmentFrom course
    to    = courseEnrollmentTo   course


------------------------------------------------------------------------------
-- | Splice that is mapped over the <assignedTasks> tag to render assignment
-- specific details into the template.
renderAssignment :: Assignment -> Splice AppHandler
renderAssignment assignment = do
    now  <- liftIO getCurrentTime
    task <- fromJust <$> (lift $ Database.getTask (assignmentTask assignment))
    solutions <- lift $ Database.getSolutionsByAssignment
                          (assignmentId assignment)  

    I.runChildrenWith [
        ("taskName",     taskName              |- task)
      , ("taskType",     taskType              |- task)
      , ("status",       translateStatus       |- assignmentStatus assignment)
      , ("timespan",     id                    |- compareToNow now from to)
      , ("submissions",  length                |< solutions)
      , ("bestscore"  ,  translateScore        |- bestScore task solutions)
      , ("scoringOrder", translateScoringOrder |- taskScoringOrder task)
      ]
  where
    from = Just $ assignmentStart assignment
    to   = Just $ assignmentEnd   assignment

