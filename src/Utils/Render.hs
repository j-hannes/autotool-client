{-# LANGUAGE OverloadedStrings #-}

module Utils.Render
  ( compareToNow
  , doubleToInt
  , translateStatus
  , (|<)
  , (|-)
  , (|.)
  ) where

------------------------------------------------------------------------------
import           Data.Text         (Text)
import qualified Data.Text         as T
import           Data.Time         (UTCTime)
------------------------------------------------------------------------------
import           Heist.Interpreted (Splice)
import qualified Heist.Interpreted as I
------------------------------------------------------------------------------
import           Application       (AppHandler)
import           Model.Types       (Status(..))

------------------------------------------------------------------------------
-- | Some syntactic sugar.
(|<) :: (Show b) => (a -> b) -> a -> Splice AppHandler
f |< x = I.textSplice . T.pack . show $ f x

(|-) :: (a -> String) -> a -> Splice AppHandler
f |- x = I.textSplice . T.pack $ f x

(|.) :: (a -> Text) -> a -> Splice AppHandler
f |. x = I.textSplice $ f x


------------------------------------------------------------------------------
-- | 
translateStatus :: Status -> String
translateStatus Mandatory = "Pflicht"
translateStatus Optional  = "Zusätzlich"


------------------------------------------------------------------------------
-- | 
compareToNow :: UTCTime -> Maybe UTCTime -> Maybe UTCTime -> String
compareToNow _   Nothing      _          = "offen"
compareToNow _   _            Nothing    = "offen"
compareToNow now (Just begin) (Just end)
    | now < begin = (show begin) ++ " - " ++ (show end)
    | now < end = "noch bis " ++ (show end)
    | otherwise = "vorbei seit " ++ (show end)


------------------------------------------------------------------------------
-- | 
doubleToInt :: Double -> Int
doubleToInt = read . takeWhile ((/=) '.') . show
