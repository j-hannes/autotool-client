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
import qualified Model.Base         as Model
import           Model.Types
import           Utils.Render       (compareToNow, bestScore, translateScore)
import           Utils.Render       (translateStatus, (|<), (|-))


------------------------------------------------------------------------------
-- | Renders the landing page with an overview over courses and assignments.
handleTutor :: AppHandler ()
handleTutor = ifTop $ do
    mTutor <- Model.getTutor 1
    case mTutor of
      Nothing -> redirect "/404"
      Just tutor -> continueWith tutor
  where
    continueWith tutor = do
      courses <- Model.getCoursesByTutor (tutorId tutor)
      let splice = I.mapSplices renderCourse courses
      heistLocal (I.bindSplice "courses" splice) $ render "tutor/index"


------------------------------------------------------------------------------
-- | Splice that is mapped over the <courses> tag to render course related
-- details into the template.
renderCourse :: Course -> Splice AppHandler
renderCourse course = do
    now         <- liftIO getCurrentTime
    assignments <- lift $ Model.getAssignmentsByCourse (courseId course)
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
    task <- fromJust <$> (lift $ Model.getTask (assignmentTask assignment))
    solutions <- lift $ Model.getSolutionsByAssignment
                          (assignmentId assignment)  
    I.runChildrenWith [
        ("taskName",    taskName |- task)
      , ("taskType",    taskType |- task)
      , ("status",      translateStatus |- assignmentStatus assignment)
      , ("timespan",    id              |- compareToNow now from to)
      , ("submissions", length          |< solutions)
      , ("bestscore"  , translateScore  |- bestScore task solutions)
      ]
  where
    from = Just $ assignmentStart assignment
    to   = Just $ assignmentEnd   assignment

