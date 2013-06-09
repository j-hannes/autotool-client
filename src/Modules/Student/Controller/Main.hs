{-# LANGUAGE OverloadedStrings #-}

------------------------------------------------------------------------------
-- | Controller for the student page.
module Modules.Student.Controller.Main
    ( handleStudent
    , handleStudentSelection
    ) where

------------------------------------------------------------------------------
import qualified Data.Text          as T
import           Data.Time          (UTCTime, getCurrentTime)
------------------------------------------------------------------------------
import           Heist.Interpreted  (Splice)
import qualified Heist.Interpreted  as I
import           Snap               (ifTop, liftIO)
import           Snap.Snaplet.Heist
------------------------------------------------------------------------------
import           Application        (AppHandler)
import qualified Model.Base         as Model
import           Model.Types
import           Utils.Auth         (getStudentId)
import           Utils.Render       (compareToNow, translateStatus)


------------------------------------------------------------------------------
handleStudentSelection :: AppHandler ()
handleStudentSelection = do
    render "student/pages/select"


------------------------------------------------------------------------------
-- | Renders the landing page with an overview of the tasks.
handleStudent :: AppHandler ()
handleStudent = ifTop $ do
    sid     <- getStudentId
    mStudent <- Model.getStudent sid
    case mStudent of
      Nothing -> do
        student <- Model.putStudent $ Student 0 [] []
        continueWith student
      Just student -> continueWith student
  where
    continueWith student = do
      now     <- liftIO $ getCurrentTime
      groups  <- Model.getStudentGroupBundles student
      let splices = [
              ("studentId", I.textSplice . T.pack $ show (studentId student))
            , ("groups",    I.mapSplices (renderGroups now) groups) 
            ]
      heistLocal (I.bindSplices splices) $ render "student/index"


------------------------------------------------------------------------------
-- | Splice that is mapped over the <groups> tag to render group related
-- details into the template.
renderGroups :: UTCTime -> GroupBundle -> Splice AppHandler
renderGroups now (group, crs, assnBundles) =
    I.runChildrenWith [
        ("groupDescription", groupNameSplice)
      , ("groupId",          groupIdSplice)
      , ("courseName",       courseNameSplice)
      , ("passCriteria",     passCriteriaSplice)
      , ("assignments",      assignmentsSplice)
      ]
  where
    groupNameSplice    = I.textSplice . T.pack        $ groupDescription group
    groupIdSplice      = I.textSplice . T.pack . show $ groupId group
    courseNameSplice   = I.textSplice . T.pack        $ courseName crs
    passCriteriaSplice = I.textSplice . T.pack . show $ coursePassCriteria crs
    assignmentsSplice  = I.mapSplices (renderAssignments now) assnBundles


------------------------------------------------------------------------------
-- | Splice that is mapped over the <assignments> tag to render assignment
-- related details into the template.
renderAssignments :: UTCTime -> AssignmentBundle -> Splice AppHandler
renderAssignments now (assignment, task, taskInstance) = do
    I.runChildrenWith [
        ("description",    descriptionSplice)
      , ("status",         statusSplice)
      , ("submissionTime", submissionTimeSplice)
      , ("taskInstanceId", taskInstanceIdSplice)
      ] 
  where
    descriptionSplice    = I.textSplice . T.pack        $ taskName task
    statusSplice         = I.textSplice . T.pack        $ status
    submissionTimeSplice = I.textSplice . T.pack        $ timeSpanString
    taskInstanceIdSplice = I.textSplice . T.pack . show $ tiid
    
    tiid           = taskInstanceId taskInstance
    status         = translateStatus $ assignmentStatus assignment
    timeSpanString = compareToNow now (Just $ assignmentStart assignment)
                                      (Just $ assignmentEnd assignment)
    
