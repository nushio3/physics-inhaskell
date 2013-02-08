{- | We give equations by their left hand sides. -}


{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Physics.ContinuumMechanics.Equations where

import           Data.Functor((<$>))
import           Data.Tensor.TypeLevel (Vec3, (!), compose, Axis(..))
import qualified Numeric.AD as AD
import           Numeric.AD.Types (AD)
import qualified Numeric.AD.Types as AD
import           Numeric.Physical.VectorCalculus (grad, laplacian)
import           Prelude hiding (pi)
import           UnitTyped.Type
                  (Value(..), U, val, mkVal,(|+|),(|-|), (|*|), (|/|), (*|), coerce,as,to, (:|),
                   Convertible'(..), MapMerge, MapNeg, MapEq, POne, PTwo, PThree,NOne, NTwo, NThree)

import           UnitTyped.SI (LengthDimension, Length, Mass, Time, Second, Meter, Gram)
import           UnitTyped.SI.Constants (pi, g', g)
import           UnitTyped.SI.Meta(Kilo)
import           UnitTyped.SI.Derived (Density, Pressure, GravitationalPotential, Pascal, Acceleration)


-- gravityPoisson :: (Fractional x) =>
--  (forall s. AD.Mode s => 
--  Vec3 ((AD s x):| Meter) -> Value  '[ '(Time, NTwo), '(Length, PTwo)] '[ '(Second, NTwo),  '(Meter, PTwo)] (AD s x))
--  -> (Vec3 (x :| Meter) -> (Value Density '[ '(Kilo Gram, POne), '(Meter, NThree) ] x)) 
--  -> (Vec3 (x :| Meter) -> (Value '[ '(Time, NTwo)] '[ '(Second, NTwo)] x)) 


gravityPoisson ::
  (Fractional x 
  , dimLen ~ LengthDimension
  , dimPot ~ '[ '(Time, NTwo), '(Length, PTwo)]
  , dimDen ~ Density
  , dimZhz ~ '[ '(Time, NTwo)] 
  
  , MapMerge dimLen dimLen  dimLen2
  , MapNeg dimLen2  dimLenNeg2
  , MapMerge dimPot dimLenNeg2 dimZhz
  , MapMerge dimDen  '[ '(Time, NTwo), '(Length, PThree), '(Mass, NOne) ] dimZhz

  , MapMerge uniLen uniLen  uniLen2
  , MapNeg uniLen2  uniLenNeg2
  , MapMerge  uniPot uniLenNeg2 uniZhz
  , MapMerge uniDen   '[ '(Second, NTwo), '(Meter, PThree), '((Kilo Gram), NOne) ] uniZhz'

  , Convertible' dimLen uniLen
  , Convertible' dimPot uniPot
  , Convertible' dimDen uniDen
  , Convertible' dimZhz uniZhz
  , Convertible' dimZhz uniZhz'

   ) =>
 (forall s. AD.Mode s => 
  Vec3 (Value dimLen uniLen (AD s x)) 
       -> Value dimPot uniPot (AD s x))

  -> (Vec3 (Value dimLen uniLen x) -> (Value dimDen uniDen x)) 

  -> (Vec3 (Value dimLen uniLen x) -> (Value dimZhz uniZhz x)) 

gravityPoisson gravitationalPotential density r
  = laplacian gravitationalPotential r 
    |-|    (4 *| pi |*| density r |*| g)

-- gravityPoisson gravitationalPotential density r
--  = laplacian gravitationalPotential r 
--


hydrostatic :: forall x .(Fractional x) =>
 (forall s. AD.Mode s => 
 Vec3 ((AD s x):| Meter) -> Value Pressure (U Pascal) (AD s x))
 -> (Vec3 (x :| Meter) -> (Value Density '[ '(Meter, NThree),  '(Kilo Gram, POne) ] x)) 
 -> (Vec3 (x :| Meter) -> Vec3 (Value Acceleration '[ '(Second, NTwo), '(Meter, POne)] x)) 
 -> (Vec3 (x :| Meter) -> Vec3 (Value Acceleration '[ '(Second, NTwo), '(Meter, POne)] x)) 
hydrostatic pressure density acceleration r
  = compose $ \i -> 
      let ret = (to ret) $ (gradP r ! i) |/| (density r) |+| (acceleration r ! i)
      in ret
  where 
    gradP =  grad pressure 


--     gradP ::
--         Vec3 (x :| Meter) ->
--         Vec3 (Value  '[ '(Time, NTwo), '(Mass, POne), '(Length, NTwo) ]
--                      '[ '(Second, NTwo), '(Kilo Gram, POne), '(Meter, NTwo) ]
--                        x)        


-- This bumps into an overlapping instance. cannot be implemented generically.

-- gravityPoisson :: 
--   forall x dimX uniX dimX2 uniX2 negDimX2 negUniX2 dimD uniD dimY uniY dimY' uniY' dimY'' uniY''.
--          (Fractional x,
--            Convertible' dimX uniX,
--            Convertible' dimY uniY,
--            Convertible' dimY' uniY',
--            Convertible' dimY'' uniY'',
--            Convertible' dimD uniD,
--            MapEq dimX LengthDimension,
--            MapEq dimD Density,
--            MapEq dimY GravitationalPotential,
--            MapEq dimY' '[ '(Time, NTwo)],
-- 
--            MapMerge dimX dimX dimX2,
-- --            MapMerge uniX uniX uniX2,
--            MapNeg  dimX2 negDimX2,
-- --            MapNeg  uniX2 negUniX2,
--            MapMerge dimY negDimX2 dimY',
-- --            MapMerge uniY negUniX2 uniY'
--            MapMerge dimD '[ '(Time, NTwo), '(Length, PThree), '(Mass, NOne) ] uniY'' ,
--            MapMerge uniD '[ '(Second, NTwo), '(Meter, PThree), '((Kilo Gram), NOne) ] uniY''
--           ) => 
--   (forall s. AD.Mode s => Vec3 (Value dimX uniX (AD s x)) -> Value dimY uniY (AD s x))
--   -> (Vec3 (Value dimX uniX x) -> (Value dimD uniD x)) 
--   -> (Vec3 (Value dimX uniX x) -> (Value dimY' uniY' x)) 
-- gravityPoisson gravitationalPotential density r
--   = laplacian gravitationalPotential r |-| 
--     (g |*| density r :: Value dimY'' uniY'' x)
--     --(4 *| pi |*| density r |*| g)

