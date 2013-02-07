-- | Vector calculus with dimensional units.

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Numeric.Physical.VectorCalculus (grad, laplacian) where

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
-- >>> let examplePotential4 :: (Fractional a) => Vec3 (a :| Meter) -> a :| Joule; examplePotential4 (Vec :~ x :~ y :~ z) = mkVal $ (val x) ^4 + (val y) ^4 + (val z) ^4


-- | Take the gradient of input function with dimensions.
--
-- >>> let x = (fmap mkVal $ vec3 1 2 3) :: Vec3 (Double :| Meter)
-- >>> grad examplePotential x
-- (2.0 J⋅m⁻¹,4.0 J⋅m⁻¹,6.0 J⋅m⁻¹)
-- >>> fmap (to newton) $ grad examplePotential x
-- (2.0 N,4.0 N,6.0 N)

grad :: forall f a dimX uniX negDimX negUniX dimY uniY dimY' uniY'.
         (Traversable f, Num a,
           Convertible' dimX uniX,
           MapNeg  dimX negDimX,
           MapNeg  uniX negUniX,
           Convertible' dimY uniY,
           Convertible' dimY' uniY',
           MapMerge dimY negDimX dimY',
           MapMerge uniY negUniX uniY'
          )
     => (forall s. AD.Mode s => f (Value dimX uniX (AD s a)) -> (Value dimY uniY (AD s a)) )
         -> f (Value dimX uniX a) -> f (Value dimY' uniY' a)
grad fun = fmap mkVal . AD.grad fun' . fmap val
  where
    fun' :: (forall s. AD.Mode s => f (AD s a) -> AD s a)
    fun' = val . fun . fmap mkVal

laplacian :: forall f a dimX uniX dimX2 uniX2 negDimX2 negUniX2 dimY uniY dimY' uniY'.
         (Traversable f, Num a,
           Convertible' dimX uniX,
           MapMerge dimX dimX dimX2,
           MapMerge uniX uniX uniX2,
           MapNeg  dimX2 negDimX2,
           MapNeg  uniX2 negUniX2,
           Convertible' dimY uniY,
           Convertible' dimY' uniY',
           MapMerge dimY negDimX2 dimY',
           MapMerge uniY negUniX2 uniY'
          )
     => (forall s. AD.Mode s => f (Value dimX uniX (AD s a)) -> (Value dimY uniY (AD s a)) )
         -> f (Value dimX uniX a) -> f (Value dimY' uniY' a)
laplacian fun = fmap mkVal . AD.grad fun' . fmap val
  where
    fun' :: (forall s. AD.Mode s => f (AD s a) -> AD s a)
    fun' = val . fun . fmap mkVal



