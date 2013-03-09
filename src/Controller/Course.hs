{-# LANGUAGE OverloadedStrings #-}

------------------------------------------------------------------------------
-- | Controller for creating and managing courses.
module Controller.Course
  ( handleCourseForm
  ) where

------------------------------------------------------------------------------
import           Data.Maybe
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time
import           Data.Time.Clock
import           Snap
import           System.Locale
import           Text.Digestive.Form
import           Text.Digestive.Snap
------------------------------------------------------------------------------
import           Application
import           Model.Adapter.File
import           Model.Types.Course (Course (Course))
import qualified Model.Types.Course as Course
--import qualified Model.User as User
import           Utils.Form


------------------------------------------------------------------------------
-- | Render a form to display a course.
handleCourseForm :: AppHandler ()
handleCourseForm = do
    time <- convertToText <$> liftIO getCurrentTime
    (view, courseData) <- runForm "form" (courseForm time)
    maybe (renderForm "tutor/forms/course" view) createCourse courseData


------------------------------------------------------------------------------
-- | Format the time into the format used in the format. (This should go into
-- a central place.
convertToText :: UTCTime -> Text
convertToText = T.pack . formatTime defaultTimeLocale "%d/%m/%Y %H:%M:%S"


------------------------------------------------------------------------------
-- | Data required in the course form.
data CourseFormData = CourseFormData
  { courseName          :: Text
  , enrollmentOpening   :: Text
  , enrollmentDeadline  :: Text
  , passCriteria        :: Double
  } deriving (Show)


------------------------------------------------------------------------------
-- | Transform the Create a new course from the entered data.
--
-- TODO Submit a user information message into the session that can then be
-- displayed on the main page instead of displaying a separate page.
createCourse :: CourseFormData -> AppHandler ()
createCourse cfd = do
    -- user       <- getUser
    -- (Just uid) <- return (fmap User.id user)
    let eo = convertDate . enrollmentOpening
        ed = convertDate . enrollmentDeadline
        course = Course {
        Course.cid             = Nothing
      , Course.courseName      = T.unpack $ courseName cfd
      , Course.semester        = "SS13"
      , Course.enrollmentBegin = eo cfd
      , Course.enrollmentEnd   = ed cfd
      , Course.passCriteria    = passCriteria cfd
      }

    _ <- liftIO $  create "course" course

    redirect "/tutor"


------------------------------------------------------------------------------
-- | Digestive course form.
courseForm :: Text -> Form Text AppHandler CourseFormData
courseForm time = CourseFormData
    <$> "courseName" .: check coursenameEmptyMsg notEmpty (text Nothing)
    <*> "enrollmentOpening"   .: (text $ Just time)
    <*> "enrollmentDeadline"  .: (text Nothing)
    <*> "passCriteria"        .: check assignInvalidMsg isPercent
                                 (stringRead "incorrect number" $ Just 0)


------------------------------------------------------------------------------
-- | Check against a valid percentage.
isPercent :: Double -> Bool
isPercent x = x <= 100


------------------------------------------------------------------------------
-- | Error message for missing course name.
coursenameEmptyMsg :: Text
coursenameEmptyMsg = "bitte Kursnamen eingeben"


------------------------------------------------------------------------------
-- | Error message for invalid assignment number input.
assignInvalidMsg :: Text
assignInvalidMsg = "ungültige Prozentangabe"
