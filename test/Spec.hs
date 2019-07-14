{-# LANGUAGE DataKinds, GADTs #-}
{-# LANGUAGE OverloadedStrings, StandaloneDeriving #-}

import System.Remote.Monitoring

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
  withMetricsStore ekgServer $ \metrics -> flip runMetricsT metrics $ do
    track Foo1
    track Foo2
    track Bar 10
    track Baz
    track Quux 42
