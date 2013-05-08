{-# LANGUAGE OverloadedStrings #-}

------------------------------------------------------------------------------
-- | Form helper library containing several functions for verification or form
-- creation.
module Utils.Form
    ( renderForm
    , notEmpty

    -- * Date time forms
    , convertDate

    ) where

------------------------------------------------------------------------------
import           Data.ByteString       (ByteString)
import           Data.Text             (Text)
import qualified Data.Text             as T
import           Data.Time.Clock
import           Data.Time.Format
import           Snap.Snaplet.Heist
import           System.Locale (defaultTimeLocale)
import           Text.Digestive.Heist
import           Text.Digestive.View
------------------------------------------------------------------------------
import           Application


------------------------------------------------------------------------------
-- | Bind the elements from the digestive form to the corresponding view
-- template.
renderForm :: ByteString -> View Text -> AppHandler ()
renderForm path view =
    heistLocal (bindDigestiveSplices view) $ render path


------------------------------------------------------------------------------
-- | Check whether a text has the length zero or not.
notEmpty :: Text -> Bool
notEmpty = not . T.null


------------------------------------------------------------------------------
-- | Converts time data types.
convertDate :: Text -> Maybe UTCTime
convertDate = parseTime defaultTimeLocale "%d/%m/%Y %H:%M:%S" . T.unpack
