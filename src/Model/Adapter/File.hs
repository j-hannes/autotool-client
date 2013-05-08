module Model.Adapter.File
  ( create
  , restoreAll
  ) where

------------------------------------------------------------------------------
import           System.IO hiding (hGetContents)
import           System.IO.Strict (hGetContents)
------------------------------------------------------------------------------
import           Model.Indexable


------------------------------------------------------------------------------
-- | Returns the filepath to a specific data file.
datafile :: String -> String
datafile name = "data/" ++ name


------------------------------------------------------------------------------
-- | Determine the next free index and store the record in a file.
create :: (Read a, Show a, Indexable a) => String -> a -> IO a
create name record = do
    records <- restoreAll name
    let nextId = getNextId 0 records
        newRecord = setId record nextId
        newRecords = newRecord : records
    handle <- openFile (datafile name) WriteMode
    hPutStr handle . unlines $ map show newRecords
    hClose handle
    return $ newRecord


------------------------------------------------------------------------------
-- | Return all stored records from a file.
restoreAll :: (Read a) => String -> IO [a]
restoreAll name = do
    handle <- openFile (datafile name) ReadMode
    contents <- hGetContents handle
    hClose handle
    return . map read $ lines contents


------------------------------------------------------------------------------
-- | Return the highest found index (id) + 1 from a list of indexable data
-- types.
getNextId :: (Indexable a) => Integer -> [a] -> Integer
getNextId n [] = n + 1
getNextId n (x:xs) | i > n     = getNextId i xs
                   | otherwise = getNextId n xs        
                   where
                     i = iid x
