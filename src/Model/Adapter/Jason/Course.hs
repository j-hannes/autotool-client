module Model.Adapter.Json.Course
  ( create
  , restoreAll
  ) where

-- import qualified Data.Aeson as JSON
import           Data.Maybe
import           System.IO hiding (hGetContents)
import           System.IO.Strict (hGetContents)

import           Model.Types.Course (Course)
import qualified Model.Types.Course as Course

datafile :: String -> String
datafile name = "data/" ++ name ++ "s.json"

------------------------------------------------------------------------------
-- | 
create :: (Indexable a) => String -> a -> IO a
create name record = do
    records <- restoreAll name
    let nextId = getNextId 0 records
        newRecord = record {iid = Just nextId}
        newRecords = newRecord : records
    handle <- openFile (datafile name) WriteMode
    hPutStr handle . unlines $ map show newRecords
    hClose handle
    return newRecord

-- restore :: Int -> IO TaskConfig
-- restore _ = undefined

restoreAll :: (Read a) => String -> IO [a]
restoreAll name = do
    handle <- openFile (datafile name) ReadMode
    contents <- hGetContents handle
    hClose handle
    return . map read $ lines contents


------------------------------------------------------------------------------
getNextId :: (Indexable a) => Int -> [a] -> Int
getNextId n [] = n + 1
getNextId n (x:xs) | isJust i && fromJust i > n = getNextId (fromJust i) xs
                   | otherwise                  = getNextId n xs        
                   where
                     i = ii x
