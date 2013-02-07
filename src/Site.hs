{-# LANGUAGE OverloadedStrings #-}

------------------------------------------------------------------------------
-- | This module is where all the routes and handlers are defined for your
-- site. The 'app' function is the initializer that combines everything
-- together and is exported by this module.
module Site
  ( app
  ) where

------------------------------------------------------------------------------
import           Data.ByteString       (ByteString)
import           Data.Monoid
import           Heist
import           Snap
import           Snap.Snaplet.Heist
import           Snap.Util.FileServe
------------------------------------------------------------------------------
import           Application
import           Controller.TaskConfig (handleTaskConfig)
import           Controller.TaskTree   (handleTaskTree)


------------------------------------------------------------------------------
-- | The application's routes.
routes :: [(ByteString, Handler App App ())]
routes = [
    ("/",                         ifTop $ render "index")
  , ("/task/select",              handleTaskTree)
  , ("/task/configure/:taskname", handleTaskConfig)
  , ("",                          serveDirectory "static")
  ]


------------------------------------------------------------------------------
-- | The application initializer.
app :: SnapletInit App App
app = makeSnaplet "app" "An snaplet example application." Nothing $ do
    h <- nestSnaplet "" heist $ heistInit' "templates" config
    addRoutes routes
    return $ App h
  where
    config = mempty { hcInterpretedSplices = defaultInterpretedSplices }
