# monad-metrics-extensible

[![Build Status][travis-badge]][travis]
[![Hackage][hackage-badge]][hackage]
[![Stackage LTS][stackage-lts-badge]][stackage-lts]
[![Stackage Nightly][stackage-nightly-badge]][stackage-nightly]

## tl;dr

This library simplifies using [ekg](http://hackage.haskell.org/package/ekg)
in three ways:

* It allows specifying metrics as constructors of a user-defined GADT
  carrying both the metric name (to avoid typos) and the metric kind
  (a counter, a distribution and so on — to avoid code duplication).
  Multiple GADTs in the same appplication are supported (hence
  "extensible").
* It encapsulates managing all the necessary EKG objects on-demand via
  a monadic API.
* It allows defining new kinds of metrics in the user code
  (hence "extensible" one more time).
  You want a combined distribution + counter? No prob!

`System.Metrics.Extensible` is your entry point of choice!

## A quick example

First we enable a few extensions and import some packages:
```haskell
{-# LANGUAGE DataKinds, GADTs, StandaloneDeriving #-}

import System.Metrics.Extensible
import System.Remote.Monitoring -- for ekg stuff
```

Then we define a type that represents the possible metrics in our
application:
```haskell
data SomeMetrics ty name where
  SomeCounter     :: SomeMetrics Counter "some_counter"
  AnotherCounter  :: SomeMetrics Counter "other_counter"
  SomeGauge       :: SomeMetrics Gauge   "some_gauge"
```
The string literals are what will be shown via ekg UI.

There is a couple of requirements:

* The type shall be of the kind `* -> Symbol -> *`.
* The first type argument (`Counter` and `Gauge` in the example above)
  shall be an instance of `TrackerLike`. All ekg counters are already
  instances of this class.
* The type shall be comparable, hence we also do
  ```haskell
  deriving instance Eq (SomeMetrics ty name)
  deriving instance Ord (SomeMetrics ty name)
  ```

Then we can write our small program!

```haskell
main :: IO ()
main = do
  ekgServer <- forkServer "localhost" 8000
  withMetricsStore ekgServer $ \store -> flip runMetricsT store $ do
    track SomeCounter
    track SomeGauge 42
```

* `withMetricsStore` creates the metrics store that's managed by this
  library and runs an `IO` computation with that store.
* `runMetricsT` is what runs the monad transformer giving access to
  the metrics.
* `track` is the function that's responsible for updating the metrics.
  Its arguments depend on the specific metric that's being tracked:
  for instance, as can be seen in the example above,
  `Counter`s have no arguments, while `Gauge`s accept
  the corresponding new value.


[travis]:        <https://travis-ci.org/0xd34df00d/monad-metrics-extensible>
[travis-badge]:  <https://travis-ci.org/0xd34df00d/monad-metrics-extensible.svg?branch=master>
[hackage]:       <https://hackage.haskell.org/package/monad-metrics-extensible>
[hackage-badge]: <https://img.shields.io/hackage/v/monad-metrics-extensible.svg>
[stackage-lts-badge]: <http://stackage.org/package/monad-metrics-extensible/badge/lts>
[stackage-nightly-badge]: <http://stackage.org/package/monad-metrics-extensible/badge/nightly>
[stackage-lts]: <http://stackage.org/lts/package/monad-metrics-extensible>
[stackage-nightly]: <http://stackage.org/nightly/package/monad-metrics-extensible>
