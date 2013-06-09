{-# LANGUAGE OverloadedStrings #-}

------------------------------------------------------------------------------
-- | Controller for the student page.
module Modules.Student.Controller.Main
    ( handleStudent
    , handleStudentSelection
    ) where

------------------------------------------------------------------------------
import           Data.Maybe         (fromJust)
import           Data.Time          (getCurrentTime)
------------------------------------------------------------------------------
import           Heist.Interpreted  (Splice)
import qualified Heist.Interpreted  as I
import           Snap               (ifTop, lift, liftIO, (<$>))
import           Snap.Snaplet.Heist
------------------------------------------------------------------------------
import           Application        (AppHandler)
import qualified Model.Base         as Model
import           Model.Types
import           Utils.Auth         (getStudentId)
import           Utils.Render       (compareToNow, translateStatus, (|<), (|-))


------------------------------------------------------------------------------
handleStudentSelection :: AppHandler ()
handleStudentSelection = do
    render "student/pages/select"


------------------------------------------------------------------------------
-- | Renders the landing page with an overview of the tasks.
handleStudent :: AppHandler ()
handleStudent = ifTop $ do
    sid      <- getStudentId
    mStudent <- Model.getStudent sid
    case mStudent of
      Nothing -> do
        student <- Model.newStudent sid
        continueWith student
      Just student -> continueWith student
  where
    continueWith student = do
      groups <- Model.getEnrolledGroups student
      let splices = [
              ("studentId", studentId |< student)
            , ("groups",    I.mapSplices renderGroup groups) 
            ]
      heistLocal (I.bindSplices splices) $ render "student/index"


------------------------------------------------------------------------------
-- | Splice that is mapped over the <groups> tag to render group related
-- details into the template.
renderGroup :: Group -> Splice AppHandler
renderGroup group = do
    course      <- fromJust <$> (lift $ Model.getCourse (groupCourseId group))
    assignments <- lift $ Model.getAssignments (courseAssignments course)
    I.runChildrenWith [
        ("groupDescription", groupDescription   |< group)
      , ("groupId",          groupId            |< group)
      , ("courseName",       courseName         |- course)
      , ("passCriteria",     coursePassCriteria |< course)
      , ("assignments",      I.mapSplices renderAssignment assignments)
      ]


------------------------------------------------------------------------------
-- | Splice that is mapped over the <assignments> tag to render assignment
-- related details into the template.
renderAssignment :: Assignment -> Splice AppHandler
renderAssignment assignment = do
    now          <- liftIO getCurrentTime
    task         <- fromJust <$> (lift $ Model.getTask
                                           (assignmentTaskId assignment))
    sid          <- lift getStudentId
    student      <- fromJust <$> (lift $ Model.getStudent sid)
    taskInstance <- lift $ Model.getCachedTaskInstance assignment student
    I.runChildrenWith [
        ("description",    taskName |- task)
      , ("status",         (translateStatus . assignmentStatus) |- assignment)
      , ("submissionTime", id |- (compareToNow now from to))
      , ("submissions"   , id |< (length $ taskInstanceSolutions taskInstance))
      , ("taskInstanceId", taskInstanceId |< taskInstance)
      ] 
  where
    from = Just $ assignmentStart assignment
    to   = Just $ assignmentEnd assignment
