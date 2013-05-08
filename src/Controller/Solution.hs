{-# LANGUAGE OverloadedStrings #-}

------------------------------------------------------------------------------
-- | Controller for creating solutions.
module Controller.Solution
    ( showSolveTaskForm
    ) where

------------------------------------------------------------------------------
import qualified Data.ByteString.Char8    as BS
import           Data.List                (group)
import           Data.List.Split          (splitOn)
import           Data.Maybe
import           Data.Text                (Text)
import qualified Data.Text                as T
import           Data.Time                (getCurrentTime)
import           Snap                     hiding (Config)
import           Snap.Snaplet.Heist
import           Text.Digestive.Form
import           Text.Digestive.Snap      hiding (method)
------------------------------------------------------------------------------
import           Application              (AppHandler)
import qualified Autotool.Client          as Autotool
import qualified Model.Adapter.File.Solution as Solution
import qualified Model.Adapter.File.TaskInstance as TaskInstance
import           Model.Types.Solution
import           Model.Types.TaskInstance
import           Utils.Auth               (getStudentId)
import           Utils.Form               (renderForm, notEmpty)
import qualified View.Solution            as View


------------------------------------------------------------------------------
-- | Handler that renders the solution config form when called via GET request
-- and processes the form input date when called via POST request. Therefore
-- the autotool backend server is asked for solution specific data. If the
-- word 'mock' is attached to the url query string, then a mocked solution
-- config is returned (can be used when not in reach of the autotool server).
showSolveTaskForm :: AppHandler ()
showSolveTaskForm = do
    tiid <- fmap BS.unpack $ fromMaybe "" <$> getParam "taskInstanceId"
    taskInstance <- liftIO $ TaskInstance.getById (read tiid)
    solutions    <- liftIO $ Solution.getAllByTaskInstanceId (read tiid)
    let lastSolution = if null solutions
                          then Nothing
                          else Just $ head solutions --FIXME
        (errEva, succEva) = determineResult lastSolution
        solutionText = fromMaybe (taskInstanceSolution taskInstance)
                                 (fmap solutionContent lastSolution)
    method GET (handleForm (taskInstanceDescription   taskInstance)
                           solutionText
                           (taskInstanceDocumentation taskInstance)
                           succEva
                           errEva)
      <|> method POST (handleFormSubmit taskInstance)

determineResult :: Maybe Solution -> (Maybe String, Maybe String)
determineResult Nothing = (Nothing, Nothing)
determineResult (Just sol)
  | isNothing (solutionResult sol) = (Just $ solutionEvaluation sol, Nothing)
  | otherwise                      = (Nothing, Just $ solutionEvaluation sol)


------------------------------------------------------------------------------
-- | Read the solution name from the url query (GET parameter) and ask the
-- autotool backend server for an example configuration, documentation and
-- solution description.
handleForm :: String
           -> String
           -> String
           -> Maybe String
           -> Maybe String
           -> AppHandler ()
handleForm taskDescription exampleSolution documentation eva err = do
    view <- fst <$> runForm "form" (solutionForm exampleSolution)
    heistLocal splices (renderForm View.solutionFormTemplate view)
  where
    splices = View.bindFormSplices taskDescription doc eva err
    doc     = read documentation


------------------------------------------------------------------------------
-- | Data required in the solution form.
data SolutionFormData = SolutionFormData
  { solutionBody :: Text
  } deriving (Show)


------------------------------------------------------------------------------
-- | Solution form. Checks if a configuration is valid.
solutionForm :: String -> Form Text AppHandler SolutionFormData
solutionForm exampleSolution = SolutionFormData
    <$> "solution" .: check "" notEmpty (text $ Just $ T.pack exampleSolution)


------------------------------------------------------------------------------
-- | Handler that checks if the reset button has been clicked. If so it
-- redirects to the form page with initial values otherwise it continues with
-- the passed handler.
handleFormSubmit :: TaskInstance -> AppHandler ()
handleFormSubmit taskInstance = do
    reset <- getParam "btn_reset"
    if isJust reset
      then redirect . BS.pack $ "/student/solve/" ++ (show $ taskInstanceId taskInstance)
      else handleFormVerification taskInstance


------------------------------------------------------------------------------
-- | Handler that is called if the form is submitted. The config is verified
-- by the autotool backend server and depending on this result and which
-- button has been pressed a verification result is shown or the solution config
-- is stored in the database backend.
handleFormVerification :: TaskInstance -> AppHandler ()
handleFormVerification taskInstance = do
    Just formData <- snd <$> runForm "form" (solutionForm undefined)

    let sol  = T.unpack (solutionBody formData)
        sig  = taskInstanceSignature     taskInstance
        desc = taskInstanceDescription   taskInstance
        tiid = taskInstanceId            taskInstance
        doc  = taskInstanceDocumentation taskInstance

    result <- liftIO $ Autotool.submitSolution sig sol
    studentId <- getStudentId

    case result of
      (Right signature) -> do
        createSolution sol signature tiid
        redirect . BS.pack $ "/student/" ++ (show studentId) ++ "/solve/"
                                         ++ show tiid  -- createUrl?
      (Left  errormsg)  -> do
        createSolution sol errormsg tiid
        handleForm desc sol doc Nothing (Just $ format errormsg)


------------------------------------------------------------------------------
-- | Create a new solution from the entered data.
createSolution :: String -> String -> Integer -> AppHandler ()
createSolution cont response tiid = do
    now <- liftIO $ getCurrentTime

    liftIO $ putStrLn response
    let (_:result) = splitOn ["Bewertung"] $ words response
    
    _ <- liftIO $ Solution.create tiid cont (format response) (getResult result) now
    return ()

format :: String -> String
format = unlines . map head . group . lines

getResult :: [[String]] -> Maybe Result
getResult [] = Nothing
getResult result | "No" `elem` (head result) = Nothing
                 | otherwise = Just $ Result sco siz
  where
    sco = read $ head $ drop 2 $ dropWhile ((/=) "punkte") $ head result
    siz  = read $ head $ drop 2 $ dropWhile ((/=) "size_") $ head result
