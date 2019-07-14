{-# LANGUAGE RankNTypes, GADTs #-}
{-# LANGUAGE StandaloneDeriving, ScopedTypeVariables, TypeOperators #-}

module System.Metrics.Store
( MetricsStore
, newMetricsStore
, withMetricsStore

, getMetricFromStore
) where

import qualified Data.Dependent.Map as DM
import qualified Data.Text as T
import Control.Concurrent
import Control.Concurrent.STM.TQueue
import Control.Monad.Catch
import Control.Monad.Identity
import Control.Monad.STM
import Data.GADT.Compare
import Data.Proxy
import Data.Typeable(eqT)
import GHC.TypeLits
import System.Remote.Monitoring
import Type.Reflection

import Data.Dyn
import System.Metrics.Monad.Class

type DynOrd = Dyn Ord

data SomeMetric tracker where
  MkSomeMetric :: (Typeable metric, TrackerLike tracker, KnownSymbol name, Ord (metric tracker name))
               => metric tracker name -> SomeMetric tracker

deriving instance Typeable (SomeMetric t)

instance GEq SomeMetric where
  geq (MkSomeMetric _) (MkSomeMetric _) = eqT

instance GCompare SomeMetric where
  gcompare sm1@(MkSomeMetric (m1 :: mTy1 tTy1 nTy1)) sm2@(MkSomeMetric (m2 :: mTy2 tTy2 nTy2)) =
    case eqT :: Maybe (tTy1 :~: tTy2) of
      Just Refl -> case compare (toDyn m1 :: DynOrd) (toDyn m2) of
        LT -> GLT
        EQ -> GEQ
        GT -> GGT
      Nothing -> case compare (someTypeRep sm1) (someTypeRep sm2) of
        LT -> GLT
        EQ -> error "SomeTypeReps are equal though eqT proved them wrong"
        GT -> GGT

data MetricsState = MetricsState
  { server :: Server
  , metrics :: DM.DMap SomeMetric Identity
  }

data MetricRequest where
  MetricRequest :: (TrackerLike tracker, KnownSymbol name, Typeable metric, Ord (metric tracker name))
                => metric tracker name
                -> MVar tracker
                -> MetricRequest

newtype MetricsStore = MetricsStore { mReqQueue :: TQueue MetricRequest }

newMetricsStore :: Server -> IO (MetricsStore, IO ())
newMetricsStore srv = do
  queue <- newTQueueIO
  threadId <- forkIO $ act queue $ MetricsState srv mempty
  pure (MetricsStore queue, killThread threadId)
  where
    act queue state = do
      req <- atomically (readTQueue queue)
      state' <- (\(MetricRequest metric mvar) -> handleReq state metric mvar) req
      act queue state'

    handleReq :: forall metric tracker name. (TrackerLike tracker, KnownSymbol name, Typeable metric, Ord (metric tracker name))
              => MetricsState
              -> metric tracker name
              -> MVar tracker
              -> IO MetricsState
    handleReq state metric mvar = do
      let asSome = MkSomeMetric metric
      (tracker, state') <- case DM.lookup asSome $ metrics state of
        Just existing -> pure (runIdentity existing, state)
        Nothing -> do
          let trackerName = symbolVal (Proxy :: Proxy name)
          newTracker <- createTracker (T.pack trackerName) $ serverMetricStore $ server state
          pure (newTracker, state { metrics = DM.insert asSome (Identity newTracker) $ metrics state })
      putMVar mvar tracker
      pure state'

withMetricsStore :: Server -> (MetricsStore -> IO a) -> IO a
withMetricsStore srv f = bracket
  (newMetricsStore srv)
  snd
  (f . fst)

getMetricFromStore :: (TrackerLike tracker, KnownSymbol name, Typeable metric, Ord (metric tracker name))
                   => MetricsStore
                   -> metric tracker name
                   -> IO tracker
getMetricFromStore store metric = do
  mvar <- newEmptyMVar
  atomically $ writeTQueue (mReqQueue store) $ MetricRequest metric mvar
  takeMVar mvar
