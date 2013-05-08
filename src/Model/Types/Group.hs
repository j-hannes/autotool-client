module Model.Types.Group where

import           Model.Indexable

data Group = Group {
    -- ^ Identifier
    groupId             :: Integer

    -- ^ Relations
  , groupCourseId       :: Integer

    -- ^ Attributes
  , groupDescription    :: String
  , groupCapacity       :: Int
  } deriving (Eq, Ord, Read, Show)


instance Indexable Group where
  iid = groupId
  setId group idVal = group { groupId = idVal }
