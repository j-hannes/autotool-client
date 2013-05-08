import           Data.IntMap (IntMap, Key)
import qualified Data.IntMap as Map
import           Data.Time (UTCTime, getCurrentTime)

fname :: String
fname = "foo"

data Tutor = Tutor String deriving (Eq, Read, Show)

data Store a = Store
  { object :: a
  , createdAt :: UTCTime
  , modifiedAt :: UTCTime
  } deriving (Eq, Read, Show)


inc :: IntMap a -> Key
inc m | null (Map.keys m) = 1
      | otherwise = maximum (Map.keys m) + 1

-- create :: IntMap Tutor -> Tutor -> IntMap Tutor

main :: IO ()
main = do
  content <- readFile fname
  time <- getCurrentTime
  let database  = read content :: IntMap (Store Tutor)
      nextId    = inc database
      storeObj  = Store (Tutor "abcd") time time
      database' = Map.insert nextId storeObj database
  print database'
  writeFile fname (show database')
  return ()
