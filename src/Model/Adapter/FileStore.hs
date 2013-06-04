module Model.Adapter.FileStore where 

------------------------------------------------------------------------------
import           System.IO          hiding (hGetContents)
import           System.IO.Strict   (hGetContents)
------------------------------------------------------------------------------
import           Snap               (liftIO)
------------------------------------------------------------------------------
import           Application        (AppHandler)
import           Model.Adapter.Base (getNextId)
import           Model.Indexable
import           Model.Types


------------------------------------------------------------------------------
-- | Returns the filepath to a specific data file.
datafile :: String -> String
datafile name = "data/" ++ name


------------------------------------------------------------------------------ 
-- | Generic getter and setter.

------------------------------------------------------------------------------
-- | Return all stored records from a file.
get :: (Read a) => String -> AppHandler [a]
get name = do
    handle   <- liftIO $ openFile (datafile name) ReadMode
    contents <- liftIO $ hGetContents handle
    liftIO $ hClose handle
    return . map read $ lines contents

------------------------------------------------------------------------------
-- | Determine the next free index and store the record in a file.
create :: (Read a, Show a, Indexable a) => String -> a -> AppHandler a
create name record = do
    records <- get name
    let nextId = getNextId 0 records
        newRecord = setId record nextId
        newRecords = newRecord : records
    handle <- liftIO $ openFile (datafile name) WriteMode
    liftIO $ hPutStr handle . unlines $ map show newRecords
    liftIO $ hClose handle
    return $ newRecord


------------------------------------------------------------------------------ 
-- | Specific getters.

getAssignments   :: AppHandler [Assignment]
getAssignments   = get "assignments"

getCourses       :: AppHandler [Course]
getCourses       = get "courses"

getEnrollments   :: AppHandler [Enrollment]
getEnrollments   = get "enrollments"

getGroups        :: AppHandler [Group]
getGroups        = get "groups"

getSolutions     :: AppHandler [Solution]
getSolutions     = get "solutions"

getTasks         :: AppHandler [Task]
getTasks         = get "tasks"

getTaskInstances :: AppHandler [TaskInstance]
getTaskInstances = get "taskInstances"


------------------------------------------------------------------------------ 
-- | Specific setters.

createAssignment   :: Assignment -> AppHandler Assignment
createAssignment   = create "assignments"

createCourse       :: Course -> AppHandler Course
createCourse       = create "courses"

createEnrollment   :: Enrollment -> AppHandler Enrollment
createEnrollment   = create "enrollments"

createGroup        :: Group -> AppHandler Group
createGroup        = create "groups"

createSolution     :: Solution -> AppHandler Solution
createSolution     = create "solutions"

createTask         :: Task -> AppHandler Task
createTask         = create "tasks"

createTaskInstance :: TaskInstance -> AppHandler TaskInstance
createTaskInstance = create "taskInstances"
