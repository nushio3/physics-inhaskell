-- | Vector calculus with dimensional units.

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Numeric.Physical.VectorCalculus (grad) where

import           Data.Traversable (Traversable)
import qualified Numeric.AD as AD
import           Numeric.AD.Types (AD)
import qualified Numeric.AD.Types as AD
import           UnitTyped (Value(..), val, mkVal,
                            Convertible'(..), MapMerge, MapNeg)

-- $setup
-- >>> :set -XTypeOperators
-- >>> import UnitTyped
-- >>> import UnitTyped.SI
-- >>> import UnitTyped.SI.Derived
-- >>> import Data.Tensor.TypeLevel (Vec3,vec3,Vec(..),(:~)(..))
-- >>> let examplePotential :: (Fractional a) => Vec3 (a :| Meter) -> a :| Joule; examplePotential (Vec :~ x :~ y :~ z) = mkVal $ (val x) ^2 + (val y) ^2 + (val z) ^2


-- | Take the gradient of input function with dimensions.
--
-- >>> let x = (fmap mkVal $ vec3 1 2 3) :: Vec3 (Double :| Meter)
-- >>> grad examplePotential x
-- (2.0 J⋅m⁻¹,4.0 J⋅m⁻¹,6.0 J⋅m⁻¹)
-- >>> fmap (to newton) $ grad examplePotential x
-- (2.0 N,4.0 N,6.0 N)

grad :: forall f a d1 u1 nd1 nu1 d2 u2 d2' u2'.
         (Traversable f, Num a,
           Convertible' d1 u1,
           MapNeg  d1 nd1,
           MapNeg  u1 nu1,
           Convertible' d2 u2,
           Convertible' d2' u2',
           MapMerge d2 nd1 d2',
           MapMerge u2 nu1 u2'
          )
     => (forall s. AD.Mode s => f (Value d1 u1 (AD s a)) -> (Value d2 u2 (AD s a)) )
         -> f (Value d1 u1 a) -> f (Value d2' u2' a)
grad fun = fmap mkVal . AD.grad fun' . fmap val
  where
    fun' :: (forall s. AD.Mode s => f (AD s a) -> AD s a)
    fun' = val . fun . fmap mkVal
