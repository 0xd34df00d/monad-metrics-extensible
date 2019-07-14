{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module System.Metrics.TrackerInstances where

import Control.Monad.IO.Class
import GHC.Int
import System.Metrics
import System.Metrics.Counter as TC
import System.Metrics.Distribution as TD
import System.Metrics.Gauge as TG

import System.Metrics.Monad.Class

instance TrackerLike Counter where
  type TrackAction Counter m = m ()
  track metric = getTracker metric >>= liftIO . TC.inc
  createTracker = createCounter

instance TrackerLike Distribution where
  type TrackAction Distribution m = Double -> m ()
  track metric val = getTracker metric >>= \distr -> liftIO $ TD.add distr val
  createTracker = createDistribution

instance TrackerLike Gauge where
  type TrackAction Gauge m = Int64 -> m ()
  track metric val = getTracker metric >>= \gauge -> liftIO $ TG.set gauge val
  createTracker = createGauge

newtype DistrGauge = DistrGauge (Distribution, Gauge)

instance TrackerLike DistrGauge where
  type TrackAction DistrGauge m = Int64 -> m ()
  track metric val = do
    DistrGauge (distr, gauge) <- getTracker metric
    liftIO $ do
      TG.add gauge val
      TD.add distr $ fromIntegral val
  createTracker name store = do
    d <- createDistribution (name <> "_distr") store
    g <- createGauge (name <> "_total") store
    pure $ DistrGauge (d, g)
