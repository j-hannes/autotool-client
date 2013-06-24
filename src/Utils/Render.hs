{-# LANGUAGE OverloadedStrings #-}

module Utils.Render
  ( compareToNow
  , translateStatus
  , translateScore
  , translateScoringOrder
  , bestScore
  , doubleToInt
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
import           Model.Datatypes

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
translateScore :: Maybe Int -> String
translateScore Nothing  = "noch keine"
translateScore (Just n) = show n

------------------------------------------------------------------------------
-- | 
translateScoringOrder :: ScoringOrder -> String
translateScoringOrder Decreasing = "absteigend"
translateScoringOrder Increasing = "aufsteigend"
translateScoringOrder None       = "keine"

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
bestScore :: Task -> [Solution] -> Maybe Int
bestScore _    []                         = Nothing
bestScore task solutions
    | taskScoringOrder task == Decreasing = maximum $ map (fmap score) results
    | taskScoringOrder task == Increasing = 
        fmap ((*)(-1)) $ maximum $ map (fmap (((*)(-1)) . score)) results
    | otherwise                           = Nothing
  where
    results = fmap solutionResult solutions
    


------------------------------------------------------------------------------
-- | Where is that actually used?
doubleToInt :: Double -> Int
doubleToInt = read . takeWhile ((/=) '.') . show
