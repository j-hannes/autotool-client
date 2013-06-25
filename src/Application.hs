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
import           Snap.Snaplet.MongoDB
import           Snap.Snaplet.SqliteSimple
------------------------------------------------------------------------------
import           Model.Datatypes



------------------------------------------------------------------------------
data App = App
    { _heist :: Snaplet (Heist App)

    , _assignments   :: IORef (Map String Assignment)
    , _courses       :: IORef (Map String Course)
    , _enrollments   :: IORef (Map String Enrollment)
    , _groups        :: IORef (Map String Group)
    , _solutions     :: IORef (Map String Solution)
    , _taskInstances :: IORef (Map String TaskInstance)
    , _tasks         :: IORef (Map String Task)
    , _tutors        :: IORef (Map String Tutor)
    , _students      :: IORef (Map String Student)

    , _db            :: Snaplet Sqlite
    , _db2           :: Snaplet MongoDB
    
    }

makeLenses ''App

instance HasHeist App where
    heistLens = subSnaplet heist

instance HasSqlite (Handler b App) where
   getSqliteState = with db get

instance HasMongoDB App where
   getMongoDB app = view snapletValue (view db2 app)

------------------------------------------------------------------------------
type AppHandler = Handler App App
