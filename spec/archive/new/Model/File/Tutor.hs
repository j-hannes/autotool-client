-- {-# LANGUAGE OrphanInstances #-}

-- TODO:
--  * refactoring: minimize code duplication in the CRUD methods
--  * modularize (move shared code to a shared module)
--  * lookup IntMap
--  * what the heck is a "orphan instance"
--  * 

module Model.File.Tutor (
    create
  , restore
  , update
  , delete
  ) where

import           Data.IntMap (IntMap, Key)
import qualified Data.IntMap as Map
import           Data.Maybe (isNothing)
import qualified System.IO.Strict as IOS

import           Model
import           Types.Tutor

fname :: String
fname = "foo"

inc :: IntMap a -> Key
inc m | null (Map.keys m) = 1
      | otherwise = maximum (Map.keys m) + 1

readDb = fmap read $ IOS.readFile fname
writeDb db = writeFile fname (show db)

instance Storable Tutor where
  create tutor = do
    database <- readDb
    let nextId    = inc database
        database' = Map.insert nextId tutor database
    writeDb database'
    return nextId

  restore tid = fmap (Map.lookup tid) readDb

  update tid tutor = do
    database <- readDb
    let lookupObj = Map.lookup tid database
    if isNothing lookupObj
      then return False
      else do
        let storeObj  = Store tutor (createdAt lookupObj) time Nothing
            database' = Map.update  -- hum
    return True 

delete :: Int -> IO ()
delete tid = do
    database <- readDb
  now     <- getCurrentTime
  let database  = read content
      database' = Map.delete tid database
  writeFile fname (show database')
