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
import           Snap               (ifTop, lift, liftIO, (<$>), redirect)
import           Snap.Snaplet.Heist
------------------------------------------------------------------------------
import           Application        (AppHandler)
import qualified Database.Switch    as Database
import           Model.Datatypes
import           Utils.Auth         (getStudentId)
import           Utils.Render       (bestScore, translateScore)
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
    mStudent <- Database.getStudent sid
    case mStudent of
      Nothing      -> redirect "/404"
      Just student -> do
        groups <- Database.getEnrolledGroups student
        let splices = [
                ("studentId", studentId |- student)
              , ("groups",    I.mapSplices renderGroup groups) 
              ]
        heistLocal (I.bindSplices splices) $ render "student/index"


------------------------------------------------------------------------------
-- | Splice that is mapped over the <groups> tag to render group related
-- details into the template.
renderGroup :: Group -> Splice AppHandler
renderGroup group = do
    course      <- fromJust <$> (lift $ Database.getCourse (groupCourse group))
    assignments <- lift $ Database.getAssignmentsByCourse (courseId course)
    I.runChildrenWith [
        ("groupDescription", groupDescription   |- group)
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
    task         <- fromJust <$> (lift $ Database.getTask
                                           (assignmentTask assignment))
    sid          <- lift getStudentId
    liftIO $ putStrLn sid
    student      <- fromJust <$> (lift $ Database.getStudent sid)
    liftIO $ print student
    taskInstance <- lift $ Database.getCachedTaskInstance assignment student
    liftIO $ print taskInstance
    solutions    <- lift $ Database.getSolutionsByAssignment
                             (assignmentId assignment)  
    mySolutions  <- lift $ Database.getSolutionsByTaskInstance
                             (taskInstanceId taskInstance)
    I.runChildrenWith [
        ("description",    taskName |- task)
      , ("status",         (translateStatus . assignmentStatus) |- assignment)
      , ("submissionTime", id |- (compareToNow now from to))
      , ("submissions"   , id |< (length mySolutions))
      , ("taskInstanceId", taskInstanceId |- taskInstance)
      , ("bestscore"  ,    translateScore  |- bestScore task solutions)
      , ("mybestscore"  ,  translateScore  |- bestScore task mySolutions)
      ] 
  where
    from = Just $ assignmentStart assignment
    to   = Just $ assignmentEnd assignment
