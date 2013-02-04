-- | Vector calculus with dimensional units.

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

module Numeric.Physical.VectorCalculus where

import           Data.Tensor.TypeLevel (Vec3,vec3,Vec(..),(:~)(..))
import           Data.Traversable (Traversable)
import qualified Numeric.AD as AD
import           Numeric.AD.Types (AD)
import qualified Numeric.AD.Types as AD
import           UnitTyped ((:|)(..), (*|),(|/|) ,(|+|), U, Value(..), square, val, mkVal)
import           UnitTyped.SI (Meter, meter, LengthDimension)

grad :: (Traversable f, Num a)
     => (forall s. AD.Mode s => f (AD s a) -> AD s a) -> f a -> f a
grad = AD.grad


grad' :: forall f a d. (Traversable f, Num a)
     => (forall s. AD.Mode s => f (AD s a :| Meter) -> (AD s a :| Meter) ) -> f (a :| Meter) -> f (a :| Meter)
grad' fun' = fmap mkVal . AD.grad fun . fmap val
  where
    fun :: (forall s. AD.Mode s => f (AD s a) -> AD s a)
    fun = val . fun' . fmap mkVal


f' :: (Fractional a) => Vec3 (a :| Meter) ->   a :| Meter
f' (Vec :~ x :~ y :~ z) = (square x |+| square y  |+| square z)|/| meter


f :: (Fractional a) => Vec3 a -> a
f = val . f' . fmap mkVal

x :: Vec3 Double
x = vec3 1 2 3

x' :: Vec3 (Double :| Meter)
x' = fmap mkVal x
