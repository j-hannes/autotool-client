{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}

------------------------------------------------------------------------------
-- | This module defines our application's state type and an alias for its
-- handler monad.
module Application where

------------------------------------------------------------------------------
import           Data.IORef                (IORef)
import           Data.Map                  (Map)
import           Control.Lens
------------------------------------------------------------------------------
import           Snap
import           Snap.Snaplet.Heist
import           Snap.Snaplet.SqliteSimple
------------------------------------------------------------------------------
import           Model.Types



------------------------------------------------------------------------------
data App = App
    { _heist :: Snaplet (Heist App)

    , _assignments   :: IORef (Map Integer Assignment)
    , _courses       :: IORef (Map Integer Course)
    , _enrollments   :: IORef (Map Integer Enrollment)
    , _groups        :: IORef (Map Integer Group)
    , _solutions     :: IORef (Map Integer Solution)
    , _taskInstances :: IORef (Map Integer TaskInstance)
    , _tasks         :: IORef (Map Integer Task)
    , _tutors        :: IORef (Map Integer Tutor)
    , _students      :: IORef (Map Integer Student)

    , _db            :: Snaplet Sqlite
    
    }

makeLenses ''App

instance HasHeist App where
    heistLens = subSnaplet heist

instance HasSqlite (Handler b App) where
   getSqliteState = with db get


------------------------------------------------------------------------------
type AppHandler = Handler App App
