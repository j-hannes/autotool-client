------------------------------------------------------------------------------
-- | A cascading Map database implementation
--
-- Each type is stored in an own Map data type which is written to an own
-- file. Supports full CRU:D operations to create, restore, update and delete
-- objects from the Maps as well as a list operation.
------------------------------------------------------------------------------

module Database where

import           Data.Map (Map) 
import qualified Data.Map as M
import qualified System.IO.Strict as IOS

import Model

type DBFile = String


------------------------------------------------------------------------------
-- LCRU-D
------------------------------------------------------------------------------

listTutorModels :: IO [TutorModel]
listTutorModels = do
    db <- readDB "tutors" :: IO (Map TutorId TutorModel)
    return $ M.elems db

createTutorModel :: Tutor  -> IO k
createTutorModel objectType obj = do
    db <- readDB objectType 
    let nextId = getNextId db
    let db' = M.insert nextId obj db
    writeDB db' objectType
    return nextId

update :: (Read a, Show a, Ord k, Read k, Show k) => DBFile -> k -> a -> IO ()
update objectType key obj = do
    db <- readDB objectType
    let db' = M.update (\_ -> Just obj) key db
    writeDB db' objectType

restore :: (Read a, Ord k, Read k) => DBFile -> k -> IO (Maybe a)
restore objectType key = do
    db <- readDB objectType
    return $ M.lookup key db

-- delete :: (Ord k, Read k, Show k) => DBFile -> k -> IO ()
-- delete objectType key = do
--    db <- readDB objectType
--    let db' = M.delete key db
--    writeDB db' objectType


------------------------------------------------------------------------------
-- UTILS
------------------------------------------------------------------------------

readDB :: (Read a, Ord k, Read k) => DBFile -> IO (Map k a)
readDB objectType = do
  contents <- IOS.readFile $ dbPath objectType
  return $ read contents

writeDB :: (Show a, Show k) => (Map k a) -> DBFile -> IO ()
writeDB db objectType = writeFile (dbPath objectType) (show db)

dbPath :: DBFile -> String
dbPath objectType = "db/" ++ objectType ++ ".map"

getNextId :: (Num k, Ord k) => Map k a -> k
getNextId m | M.null m = 1
            | otherwise = maximum (M.keys m) + 1
