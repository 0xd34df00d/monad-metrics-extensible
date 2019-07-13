{-# LANGUAGE RankNTypes, GADTs, ConstraintKinds, FlexibleInstances #-}

module Data.Dyn where

import Type.Reflection

data Dyn ctx where
  Dyn :: ctx a => TypeRep a -> a -> Dyn ctx

toDyn :: (Typeable a, ctx a) => a -> Dyn ctx
toDyn val = Dyn (typeOf val) val

withDyns :: Dyn ctx -> Dyn ctx ->
            (forall a. ctx a => a -> a -> b) ->
            (SomeTypeRep -> SomeTypeRep -> b) -> b
withDyns (Dyn ty1 v1) (Dyn ty2 v2) f def = case eqTypeRep ty1 ty2 of
  Nothing -> def (SomeTypeRep ty1) (SomeTypeRep ty2)
  Just HRefl -> f v1 v2

instance Eq (Dyn Ord) where
  d1 == d2 = withDyns d1 d2 (==) (\_ _ -> False)

instance Ord (Dyn Ord) where
  compare d1 d2 = withDyns d1 d2 compare compare
