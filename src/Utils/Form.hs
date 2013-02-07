{-# LANGUAGE OverloadedStrings #-}

------------------------------------------------------------------------------
-- | Form helper library containing several functions for verification or form
-- creation.
module Utils.Form
    ( renderForm
    , notEmpty

    -- * Date time forms
    , DateTime
    , convertDate
    , dateTimeForm

    ) where

------------------------------------------------------------------------------
import qualified Data.ByteString.Char8 as BS
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time.Clock
import           Data.Time.Format
import           Snap
import           Snap.Snaplet.Heist
import           System.Locale (defaultTimeLocale)
import           Text.Digestive.Heist
import           Text.Digestive.Form
import           Text.Digestive.View
------------------------------------------------------------------------------
import           Application


------------------------------------------------------------------------------
-- | Bind the elements from the digestive form to the corresponding view
-- template.
renderForm :: String -> View Text -> AppHandler ()
renderForm name view =
    heistLocal (bindDigestiveSplices view) $ render template
  where
    template = BS.pack $ "forms/" ++ name


------------------------------------------------------------------------------
-- | Check whether a text has the length zero or not.
notEmpty :: Text -> Bool
notEmpty = not . T.null


------------------------------------------------------------------------------
-- | Could be put even further in a date form lib or something.
data DateTime = DateTime Int Int Int Int Int deriving (Show)


------------------------------------------------------------------------------
-- | Converts time data types.
convertDate :: DateTime -> Maybe UTCTime
convertDate (DateTime dy mh yr hr me) =
    parseTime defaultTimeLocale "%m %d %Y %H %M" timeString
  where
    timeString = unwords $ map leadZero [dy, mh, yr, hr, me]
    leadZero n | n < 10    = '0' : show n
               | otherwise =       show n


------------------------------------------------------------------------------
-- | Date form with optional passed time for already filled in time.
dateTimeForm :: Monad m => Maybe UTCTime -> Form Text m DateTime
dateTimeForm time = check dateTimeInvalidMsg validDateTime $ DateTime
    <$> "day"     .: stringRead "Kein gültiger Tag" day
    <*> "month"   .: stringRead "Kein gültiger Monat" month
    <*> "year"    .: stringRead "Kein gültiges Jahr" year
    <*> "hour"    .: stringRead "Keine gültige Stunde" hour
    <*> "minute"  .: stringRead "Keine gültige Minute" minute
  where
    day    = format time "%d"
    month  = format time "%m"
    year   = format time "%Y"
    hour   = format time "%H"
    minute = format time "%M"
    
    format Nothing  _  = Nothing
    format (Just t) fs = Just . read $ formatTime defaultTimeLocale fs t

    validDateTime (DateTime day' month' year' hour' minute') =
        day'    >= 1    && day'    <= 31   &&
        month'  >= 1    && month'  <= 12   &&
        year'   >= 2012 && year'   <= 3000 &&
        hour'   >= 0    && hour'   <= 59   &&
        minute' >= 0    && minute' <= 59


------------------------------------------------------------------------------
-- | Error message for invalid date.
dateTimeInvalidMsg :: Text
dateTimeInvalidMsg = "ungültiges Datum oder ungültige Uhrzeit"
