{-# LANGUAGE DataKinds, GADTs #-}
{-# LANGUAGE OverloadedStrings, StandaloneDeriving #-}

import Control.Monad.IO.Class
import Data.Functor
import System.CPUTime
import System.Metrics.Counter as TC(read)
import System.Metrics.Distribution as TD(read, sum, count)
import System.Metrics.Gauge as TG(read)
import System.Remote.Monitoring
import Test.Hspec

import System.Metrics.Extensible
import System.Metrics.ExtraTrackers

data TestMetrics ty name where
  Foo1    :: TestMetrics Counter        "foo1"
  Foo2    :: TestMetrics Counter        "foo2"
  Bar     :: TestMetrics Gauge          "bar"
  TimeMS  :: TestMetrics (Timer 'Msecs) "timeMS"

deriving instance Eq (TestMetrics ty name)
deriving instance Ord (TestMetrics ty name)

data OtherMetrics ty name where
  Baz  :: OtherMetrics Counter "other-foo1"
  Quux :: OtherMetrics Gauge   "other-bar"

deriving instance Eq (OtherMetrics ty name)
deriving instance Ord (OtherMetrics ty name)

cpuThreadDelay :: Int -> IO ()
cpuThreadDelay usecs | usecs < 0 = pure ()
                     | otherwise = do
  start <- getCPUTime
  void $ readFile "/dev/null"
  end <- getCPUTime
  cpuThreadDelay $ usecs - fromIntegral ((end - start) `div` 1000000)

main :: IO ()
main = do
  ekgServer <- forkServer "localhost" 21000
  withMetricsStore ekgServer $ \store -> flip runMetricsT store $ do
    track Foo1
    track Foo1
    track Foo2
    track Bar 10
    track Bar 20
    track Baz
    timed TimeMS $ liftIO $ cpuThreadDelay testMicroseconds
    track Quux 42
    liftIO $ hspec $ do
      describe "Counters are stored as expected" $ do
        it "Foo1 is incremented twice" $ (getMetricFromStore store Foo1 >>= TC.read) `shouldReturn` 2
        it "This does not affect Foo2" $ (getMetricFromStore store Foo2 >>= TC.read) `shouldReturn` 1
        it "Metrics from other type are also saved" $ (getMetricFromStore store Baz >>= TC.read) `shouldReturn` 1
      describe "Gauges are stored as expected" $ do
        it "The final value of Bar is kept" $ (getMetricFromStore store Bar >>= TG.read) `shouldReturn` 20
        it "Metrics from other type are also saved" $ (getMetricFromStore store Quux >>= TG.read) `shouldReturn` 42
      describe "Timers are stored and computed as expected" $ do
        it "TimeMS has only seen one value" $ do
          storedStats <- getMetricFromStore store TimeMS >>= TD.read . getTimerDistribution
          TD.count storedStats `shouldBe` 1
        it "The value of TimeMS exceeds the wait time" $ do
          storedStats <- getMetricFromStore store TimeMS >>= TD.read . getTimerDistribution
          TD.sum storedStats `shouldSatisfy` (>= (fromIntegral testMicroseconds / 1000))
        it "…but not too much" $ do
          storedStats <- getMetricFromStore store TimeMS >>= TD.read . getTimerDistribution
          TD.sum storedStats `shouldSatisfy` (< 1000 * (fromIntegral testMicroseconds / 1000))
  where
    testMicroseconds = 10000