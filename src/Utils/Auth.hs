{-# LANGUAGE OverloadedStrings #-}

------------------------------------------------------------------------------
module Utils.Auth
  ( getStudentId
  ) where

------------------------------------------------------------------------------
import qualified Data.ByteString.Char8 as BS
import           Data.Maybe            (fromMaybe)

import           Application
import           Snap                  ((<$>), getParam)

------------------------------------------------------------------------------
-- | 
getStudentId :: AppHandler Integer
getStudentId = do
    studentId <- BS.unpack <$> fromMaybe "0" <$> getParam "studentId"
    return $ read studentId

