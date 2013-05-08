{-# LANGUAGE OverloadedStrings #-}

------------------------------------------------------------------------------
-- | Controller for creating tasks.
module Controller.TaskConfig
    ( handleTaskConfig
    ) where

------------------------------------------------------------------------------
import qualified Data.ByteString.Char8  as BS
import           Data.Maybe
import           Data.Text              (Text)
import qualified Data.Text              as T
import           Data.Time              (getCurrentTime)
import           Snap                   hiding (Config)
import           Snap.Snaplet.Heist
import           Text.Digestive.Form
import           Text.Digestive.Snap    hiding (method)
------------------------------------------------------------------------------
import           Application            (AppHandler)
import qualified Autotool.Client        as Autotool
import           Autotool.Client.Types.ScoringOrder (ScoringOrder)
import qualified Autotool.Mock          as AutotoolMock
import           Model.Adapter.File.Task as Task
import           Utils.Form             (renderForm, notEmpty)
import qualified View.Task              as View


------------------------------------------------------------------------------
-- | Handler that renders the task config form when called via GET request and
-- processes the form input date when called via POST request. Therefore the
-- autotool backend server is asked for task specific data. If the word 'mock'
-- is attached to the url query string, then a mocked task config is returned
-- (can be used when not in reach of the autotool server).
--
handleTaskConfig :: AppHandler ()
handleTaskConfig = do
    request <- getRequest
    let mock = BS.isInfixOf "mock"  $ rqQueryString request
    name <- fmap BS.unpack $ fromMaybe "" <$> getParam "taskname"
    (cfg, doc, _) <- liftIO $ if mock
                                then AutotoolMock.getInitialTaskConfig
                                else Autotool.getInitialTaskConfig name
    method GET (handleForm name cfg doc Nothing)
      <|> method POST (handleFormSubmit name)


------------------------------------------------------------------------------
-- | Read the task name from the url query (GET parameter) and ask the
-- autotool backend server for an example configuration, documentation and
-- task description.
handleForm :: String
           -> String
           -> [(String, String)]
           -> Maybe String
           -> AppHandler ()
handleForm name config doc err = do
    view <- fst <$> runForm "form" (taskForm name config)
    heistLocal splices (renderForm View.taskFormTemplate view)
  where
    splices = View.bindFormSplices name "" doc err


------------------------------------------------------------------------------
-- | Data required in the task form.
data TaskFormData = TaskFormData
  { customName   :: Text
  , customConfig :: Text
  } deriving (Show)


------------------------------------------------------------------------------
-- | Task form. Checks if a configuration is valid.
taskForm :: String -> String -> Form Text AppHandler TaskFormData
taskForm nme cfg = TaskFormData
    <$> "name"   .: check "" notEmpty (text $ Just $ T.pack nme)
    <*> "config" .: check "" notEmpty (text $ Just $ T.pack cfg)


------------------------------------------------------------------------------
-- | Handler that checks if the reset button has been clicked. If so it
-- redirects to the form page with initial values otherwise it continues with
-- the passed handler.
handleFormSubmit :: String -> AppHandler ()
handleFormSubmit name = do
    reset <- getParam "btn_reset"
    if isJust reset
      then redirect . BS.pack $ "/task/configure/" ++ name
      else handleFormVerification name


------------------------------------------------------------------------------
-- | Handler that is called if the form is submitted. The config is verified
-- by the autotool backend server and depending on this result and which
-- button has been pressed a verification result is shown or the task config
-- is stored in the database backend.
handleFormVerification :: String -> AppHandler ()
handleFormVerification taskname = do
    Just formData <- snd <$> runForm "form" (taskForm undefined undefined)

    let config = T.unpack (customConfig formData)
        ttitle = T.unpack (customName   formData)

    request <- getRequest
    let mock        = BS.isInfixOf "mock"  $ rqQueryString request
        withSuccess = not . BS.isInfixOf "error" $ rqQueryString request

    result       <- liftIO $ if mock
                               then AutotoolMock.submitTaskConfig withSuccess
                               else Autotool.submitTaskConfig taskname config
    (_, doc, so) <- liftIO $ if mock
                               then AutotoolMock.getInitialTaskConfig
                               else Autotool.getInitialTaskConfig taskname

    case result of
      (Right signature) -> createTask taskname ttitle signature so
      (Left  errormsg)  -> handleForm taskname config doc (Just errormsg)


------------------------------------------------------------------------------
-- | Create a new task from the entered data.
createTask :: String -> String -> String -> ScoringOrder -> AppHandler ()
createTask taskname tasktitle signature so = do
    userId <- return 1  -- FIXME
    now <- liftIO $ getCurrentTime
    
    _ <- liftIO $ Task.create userId tasktitle taskname signature so now
    --writeBS $ BS.pack $ show task
    
    redirect "/tutor"

