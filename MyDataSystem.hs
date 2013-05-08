import Data.Time

data ExampleDT = ExampleDT
  { name :: String
  , capacity :: Int
  } deriving (Read, Show)

data Storable a = Storable
  { index   :: Int
  , updated :: UTCTime
  , created :: UTCTime
  , content ::  a
  } deriving (Read, Show)  -- TODO removed later

class Relatable a where
  hasOne  :: [(String, a -> b)]
  hasMany :: [(String, a -> c)]
