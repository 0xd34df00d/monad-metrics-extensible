{-# LANGUAGE PolyKinds, FlexibleInstances, MultiParamTypeClasses, UndecidableInstances #-}

module System.Metrics.Monad
( MonadMetrics(..)
, MetricsT
, runMetricsT
, Metrics
) where

import Control.Monad.Catch
import Control.Monad.Identity
import Control.Monad.IO.Class
import Control.Monad.Reader

import System.Metrics.Monad.Class
import System.Metrics.Store

newtype MetricsT (m :: k -> *) (a :: k) = MetricsT { runMetricsT :: MetricsStore -> m a }
type Metrics = MetricsT Identity

instance Functor m => Functor (MetricsT m) where
  fmap f (MetricsT m) = MetricsT $ fmap f . m

instance Applicative m => Applicative (MetricsT m) where
  pure = MetricsT . const . pure
  (MetricsT fun) <*> (MetricsT val) = MetricsT $ \store -> fun store <*> val store

instance Monad m => Monad (MetricsT m) where
  (MetricsT val) >>= f = MetricsT $ \store -> val store >>= \a -> runMetricsT (f a) store

instance MonadIO m => MonadMetrics (MetricsT m) where
  getTracker metric = MetricsT $ \store -> liftIO $ getMetricFromStore store metric


instance MonadTrans MetricsT where
  lift m = MetricsT $ const m


instance MonadIO m => MonadIO (MetricsT m) where
  liftIO act = MetricsT $ const $ liftIO act

instance MonadReader r m => MonadReader r (MetricsT m) where
  ask = MetricsT $ const ask
  reader f = MetricsT $ const $ reader f
  local m (MetricsT rFun) = MetricsT $ local m . rFun

instance MonadThrow m => MonadThrow (MetricsT m) where
  throwM ex = MetricsT $ const $ throwM ex

instance MonadCatch m => MonadCatch (MetricsT m) where
  catch (MetricsT act) handler = MetricsT $ \store -> catch (act store) $ \ex -> runMetricsT (handler ex) store
