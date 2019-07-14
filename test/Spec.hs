{-# LANGUAGE DataKinds, GADTs #-}
{-# LANGUAGE OverloadedStrings, StandaloneDeriving #-}

import Control.Monad.IO.Class
import System.Metrics.Counter as TC(read)
import System.Metrics.Gauge as TG(read)
import System.Remote.Monitoring
import Test.Hspec

import System.Metrics.Extensible

data TestMetrics ty name where
  Foo1 :: TestMetrics Counter "foo1"
  Foo2 :: TestMetrics Counter "foo2"
  Bar  :: TestMetrics Gauge   "bar"

deriving instance Eq (TestMetrics ty name)
deriving instance Ord (TestMetrics ty name)

data OtherMetrics ty name where
  Baz  :: OtherMetrics Counter "foo1"
  Quux :: OtherMetrics Gauge   "bar"

deriving instance Eq (OtherMetrics ty name)
deriving instance Ord (OtherMetrics ty name)

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
    track Quux 42
    liftIO $ hspec $ do
      describe "Counters are stored as expected" $ do
        it "Foo1 is incremented twice" $ (getMetricFromStore store Foo1 >>= TC.read) `shouldReturn` 2
        it "This does not affect Foo2" $ (getMetricFromStore store Foo2 >>= TC.read) `shouldReturn` 1
        it "Metrics from other type are also saved" $ (getMetricFromStore store Baz >>= TC.read) `shouldReturn` 1
      describe "Gauges are stored as expected" $ do
        it "The final value of Bar is kept" $ (getMetricFromStore store Bar >>= TG.read) `shouldReturn` 20
        it "Metrics from other type are also saved" $ (getMetricFromStore store Quux >>= TG.read) `shouldReturn` 42
