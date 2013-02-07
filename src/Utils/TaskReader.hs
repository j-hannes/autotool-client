module Utils.TaskReader
    ( getInitialTaskConfig
    , verifyTaskConfig
    ) where


------------------------------------------------------------------------------
import           Autotool.Client as Autotool


------------------------------------------------------------------------------
-- | Procedure to get the initial config and a description from the server.
getInitialTaskConfig :: TaskName
                     -> IO (String, String, TaskDescription)
getInitialTaskConfig taskname = do
    (config, documentation) <- AC.getInitialTaskConfig taskname
    (Right signature)       <- AC.verifyTaskConfig taskname config
    (_, description, _, _)  <- AC.createTaskInstance signature ""
    return (config, description, documentation)


------------------------------------------------------------------------------
-- | Procedure to get the initial config and a description from the server.
verifyTaskConfig :: TaskName
                 -> Config
                 -> IO (Either (ErrorDescription, TaskDescription) (Signature))
verifyTaskConfig taskname config = do
    verificationResult <- AC.verifyTaskConfig taskname config
    case verificationResult of
      (Left errorMsg) -> do
        (initConf, _)           <- AC.getInitialTaskConfig taskname
        (Right signature)       <- AC.verifyTaskConfig taskname initConf
        (_, description, _, _)  <- AC.createTaskInstance signature ""
        return $ Left (errorMsg, description)
      (Right signature) -> return (Right signature)
