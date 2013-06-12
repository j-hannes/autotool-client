{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}

------------------------------------------------------------------------------
-- | This module defines our application's state type and an alias for its
-- handler monad.
module Application where

------------------------------------------------------------------------------
import           Control.Lens
------------------------------------------------------------------------------
import           Snap
import           Snap.Snaplet.Heist

------------------------------------------------------------------------------
-- To enable the Model.DbAdapter.Sqlite:
------------------------------------------------------------------------------
{-import           Snap.Snaplet.SqliteSimple-}


------------------------------------------------------------------------------
data App = App
    { _heist :: Snaplet (Heist App)

------------------------------------------------------------------------------
-- To enable the Model.DbAdapter.Sqlite:
------------------------------------------------------------------------------
    {-, _db    :: Snaplet Sqlite-}
    
    }

makeLenses ''App

instance HasHeist App where
    heistLens = subSnaplet heist

------------------------------------------------------------------------------
-- To enable the Model.DbAdapter.Sqlite:
------------------------------------------------------------------------------
{-instance HasSqlite (Handler b App) where-}
  {-getSqliteState = with db get-}


------------------------------------------------------------------------------
type AppHandler = Handler App App
