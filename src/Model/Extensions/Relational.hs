{-# OPTIONS_GHC -fno-warn-orphans #-}

module Model.Extensions.Relational where

------------------------------------------------------------------------------
import           Control.Applicative ((<$>), (<*>))
import qualified Data.Text as T
------------------------------------------------------------------------------
import           Database.SQLite.Simple
import           Database.SQLite.Simple.FromField
import           Database.SQLite.Simple.ToField
import           Database.SQLite.Simple.Internal
import           Database.SQLite.Simple.Ok
------------------------------------------------------------------------------
import           Model.Datatypes

instance FromRow Assignment where
  fromRow = Assignment
              <$> (fmap show (field :: RowParser Int))
              <*> (fmap show (field :: RowParser Int))
              <*> (fmap show (field :: RowParser Int))
              <*> field
              <*> field
              <*> field

instance FromRow Course where
  fromRow = Course
              <$> (fmap show (field :: RowParser Int))
              <*> field
              <*> field
              <*> field
              <*> field
              <*> field
              <*> field

instance FromRow Enrollment where
  fromRow = Enrollment
              <$> (fmap show (field :: RowParser Int))
              <*> (fmap show (field :: RowParser Int))
              <*> field
              <*> field

instance FromRow Group where
  fromRow = Group
              <$> (fmap show (field :: RowParser Int))
              <*> (fmap show (field :: RowParser Int))
              <*> field
              <*> field

instance FromRow Solution where
  fromRow = Solution
              <$> (fmap show (field :: RowParser Int))
              <*> (fmap show (field :: RowParser Int))
              <*> field
              <*> field
              <*> field
              <*> field

instance FromRow Student where
  fromRow = Student
              <$> field
              <*> field

instance FromRow Task where
  fromRow = Task
              <$> (fmap show (field :: RowParser Int))
              <*> field
              <*> field
              <*> field
              <*> field
              <*> field
              <*> field

instance FromRow TaskInstance where
  fromRow = TaskInstance
              <$> (fmap show (field :: RowParser Int))
              <*> (fmap show (field :: RowParser Int))
              <*> field
              <*> field
              <*> field
              <*> field
              <*> field

instance FromRow Tutor where
  fromRow = Tutor
              <$> field
              <*> field

instance FromField Result where
  fromField (Field (SQLText text) _) = Ok . read . T.unpack $ text
  fromField f = returnError ConversionFailed f "expected text"

instance FromField Status where
  fromField (Field (SQLText text) _) = Ok . read . T.unpack $ text
  fromField f = returnError ConversionFailed f "expected text"

instance FromField ScoringOrder where
  fromField (Field (SQLText text) _) = Ok . read . T.unpack $ text
  fromField f = returnError ConversionFailed f "expected text"

instance ToField Result where
  toField = SQLText . T.pack . show

instance ToField Status where
  toField = SQLText . T.pack . show

instance ToField ScoringOrder where
  toField = SQLText . T.pack . show


