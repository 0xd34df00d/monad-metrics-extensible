{-# LANGUAGE TypeFamilies #-}

module System.Metrics.ExtraTrackers where

import qualified Data.Text as T
import Control.Monad.IO.Class
import Data.Time.Clock.POSIX
import System.Metrics
import System.Metrics.Label as TL
import System.Metrics.Extensible

newtype Timestamp = Timestamp Label

instance TrackerLike Timestamp where
  type TrackAction Timestamp m = m ()
  track metric = do
    Timestamp l <- getTracker metric
    liftIO $ do
      utc <- getCurrentTime
      TL.set l $ T.pack $ show utc
  createTracker name store = Timestamp <$> createLabel name store
