{-# LANGUAGE TemplateHaskell #-}

------------------------------------------------------------------------------
-- | This module defines our application's state type and an alias for its
-- handler monad.
module Application where

------------------------------------------------------------------------------
import           Control.Lens
import           Data.IORef
import           Snap.Snaplet
import           Snap.Snaplet.Heist
import           Model.Types

------------------------------------------------------------------------------
data App = App
    { _heist         :: Snaplet (Heist App)
    , _courses       :: IORef [Course]
    , _groups        :: IORef [Group]
    , _enrollments   :: IORef [Enrollment]
    , _tasks         :: IORef [Task]
    , _assignments   :: IORef [Assignment]
    , _taskInstances :: IORef [TaskInstance]
    , _solutions     :: IORef [Solution]
    }

makeLenses ''App

instance HasHeist App where
    heistLens = subSnaplet heist


------------------------------------------------------------------------------
type AppHandler = Handler App App
