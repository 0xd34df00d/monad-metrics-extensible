{-# LANGUAGE TypeFamilies, DataKinds, ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module System.Metrics.ExtraTrackers
( Timestamp

, TimerMagnitude(..)
, Timer
, timed
) where

import qualified Data.Text as T
import Control.Monad.IO.Class
import Data.Proxy
import Data.Time.Clock.POSIX
import Data.Typeable
import GHC.TypeLits
import System.CPUTime
import System.Metrics
import System.Metrics.Distribution as TD
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


data TimerMagnitude = Msecs | Usecs | Nsecs deriving (Typeable)

class MagnitudeOps (magn :: TimerMagnitude) where
  toString :: Proxy magn -> T.Text
  secFraction :: Proxy magn -> Double

instance MagnitudeOps 'Msecs where
  toString _ = "ms"
  secFraction _ = 1e-3

instance MagnitudeOps 'Usecs where
  toString _ = "us"
  secFraction _ = 1e-6

instance MagnitudeOps 'Nsecs where
  toString _ = "ns"
  secFraction _ = 1e-9

newtype Timer (magn :: TimerMagnitude) = Timer Distribution

instance (Typeable magn, MagnitudeOps magn) => TrackerLike (Timer magn) where
  type TrackAction (Timer magn) m = Double -> m ()
  track metric timing = do
    Timer timer <- getTracker metric
    liftIO $ TD.add timer timing
  createTracker name store = Timer <$> createDistribution (name <> "_" <> toString (Proxy :: Proxy magn)) store

-- Returns the number of picoseconds
time :: MonadIO m => m a -> m (Integer, a)
time act = do
  start <- liftIO getCPUTime
  res <- act
  end <- liftIO getCPUTime
  pure (end - start, res)

timed :: forall m metric magn name a.
         (MonadMetrics m, KnownSymbol name, Typeable metric, Typeable magn, MagnitudeOps magn, Ord (metric (Timer magn) name))
      => metric (Timer magn) name
      -> m a
      -> m a
timed metric act = do
  (cpuTime, res) <- time act
  track metric $ fromIntegral cpuTime / (1e12 * secFraction (Proxy :: Proxy magn))
  pure res
