{-# LANGUAGE TypeFamilyDependencies #-}

module System.Metrics.Monad.Class where

import qualified Data.Text as T
import Control.Monad.IO.Class
import GHC.TypeLits
import System.Metrics
import Type.Reflection

class Typeable tracker => TrackerLike tracker where
  type TrackAction tracker (m :: * -> *) = r | r -> m
  track :: (MonadMetrics m, KnownSymbol name, Typeable metric, Ord (metric tracker name)) => metric tracker name -> TrackAction tracker m
  createTracker :: T.Text -> Store -> IO tracker

class MonadIO m => MonadMetrics m where
  getTracker :: (TrackerLike tracker, KnownSymbol name, Typeable metric, Ord (metric tracker name))
             => metric tracker name -> m tracker
