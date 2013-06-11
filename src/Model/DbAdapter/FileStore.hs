module Model.DbAdapter.FileStore where 

------------------------------------------------------------------------------
import           Data.Map             (Map)
import qualified Data.Map             as Map
import           System.IO            hiding (hGetContents)
import           System.IO.Strict     (hGetContents)
------------------------------------------------------------------------------
import           Snap                 (liftIO)
------------------------------------------------------------------------------
import           Application          (AppHandler)
import           Model.DbAdapter.Base (getNextId)
import           Model.Indexable
import           Model.Types


------------------------------------------------------------------------------
-- | Returns the filepath to a specific data file.
datafile :: String -> String
datafile name = "data/" ++ name


loadMap :: (Read a) => String -> AppHandler (Map Integer a)
loadMap name = do
    handle   <- liftIO $ openFile (datafile name) ReadMode
    contents <- liftIO $ hGetContents handle
    liftIO $ hClose handle
    return $ Map.fromList (map read $ lines contents)

------------------------------------------------------------------------------ 
-- | Generic getter and setter.
get :: (Read a) => String -> Integer -> AppHandler (Maybe a)
get name oid = do
    objectMap <- loadMap name
    return $ Map.lookup oid objectMap

------------------------------------------------------------------------------
-- | Return all stored records from a file.
list :: (Read a) => String -> AppHandler [a]
list name = do
    objectMap <- loadMap name
    return . Map.elems $ objectMap

------------------------------------------------------------------------------
-- | Determine the next free index and store the record in a file.
put :: (Read a, Show a, Indexable a) => String -> a -> AppHandler a
put name object = do
    objectMap <- loadMap name
    let newObject  = constructOrCopy object objectMap
        newObjects = Map.insert (iid newObject) newObject objectMap
    handle <- liftIO $ openFile (datafile name) WriteMode
    liftIO $ hPutStr handle . unlines $ map show (Map.toList newObjects)
    liftIO $ hClose handle
    return $ newObject
  where
    constructOrCopy obj objs
      | iid object == 0 = setId object (getNextId objs)
      | otherwise       = obj


------------------------------------------------------------------------------ 
-- | Specific getters.

getAssignment   :: Integer ->  AppHandler (Maybe Assignment)
getAssignment   = get "assignments"

getCourse       :: Integer -> AppHandler (Maybe Course)
getCourse       = get "courses"

getCourses      :: AppHandler [Course]
getCourses      = list "courses"

getEnrollment   :: Integer -> AppHandler (Maybe Enrollment)
getEnrollment   = get "enrollments"

getGroup        :: Integer -> AppHandler (Maybe Group)
getGroup        = get "groups"

getSolution     :: Integer -> AppHandler (Maybe Solution)
getSolution     = get "solutions"

getStudent      :: Integer -> AppHandler (Maybe Student)
getStudent      = get "students"

getTask         :: Integer -> AppHandler (Maybe Task)
getTask         = get "tasks"

getTutor        :: Integer -> AppHandler (Maybe Tutor)
getTutor        = get "tutors"

getTaskInstance :: Integer -> AppHandler (Maybe TaskInstance)
getTaskInstance = get "taskInstances"


------------------------------------------------------------------------------ 
-- | Specific setters.

putAssignment   :: Assignment -> AppHandler Assignment
putAssignment   = put "assignments"

putCourse       :: Course -> AppHandler Course
putCourse       = put "courses"

putEnrollment   :: Enrollment -> AppHandler Enrollment
putEnrollment   = put "enrollments"

putGroup        :: Group -> AppHandler Group
putGroup        = put "groups"

putSolution     :: Solution -> AppHandler Solution
putSolution     = put "solutions"

putStudent      :: Student -> AppHandler Student
putStudent      = put "students"

putTask         :: Task -> AppHandler Task
putTask         = put "tasks"

putTutor        :: Tutor -> AppHandler Tutor
putTutor        = put "tutors"

putTaskInstance :: TaskInstance -> AppHandler TaskInstance
putTaskInstance = put "taskInstances"
