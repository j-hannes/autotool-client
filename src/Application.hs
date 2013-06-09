{-# LANGUAGE TemplateHaskell #-}

------------------------------------------------------------------------------
-- | This module defines our application's state type and an alias for its
-- handler monad.
module Application where

------------------------------------------------------------------------------
import           Control.Lens
import           Data.IORef
import           Data.Map
------------------------------------------------------------------------------
import           Snap.Snaplet
import           Snap.Snaplet.Heist
------------------------------------------------------------------------------
import           Model.Types


------------------------------------------------------------------------------
data App = App
    { _heist         :: Snaplet (Heist App)
    , _assignments   :: IORef (Map Integer Assignment)
    , _courses       :: IORef (Map Integer Course)
    , _enrollments   :: IORef (Map Integer Enrollment)
    , _groups        :: IORef (Map Integer Group)
    , _solutions     :: IORef (Map Integer Solution)
    , _students      :: IORef (Map Integer Student)
    , _taskInstances :: IORef (Map Integer TaskInstance)
    , _tasks         :: IORef (Map Integer Task)
    , _tutors        :: IORef (Map Integer Tutor)
    }

makeLenses ''App

instance HasHeist App where
    heistLens = subSnaplet heist


------------------------------------------------------------------------------
type AppHandler = Handler App App
