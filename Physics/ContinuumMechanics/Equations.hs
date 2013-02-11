{- | We give equations by their left hand sides. -}


{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

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


gravityPoisson ::
  (Fractional x
  , dimLen ~ LengthDimension
  , dimPot ~ '[ '(Time, NTwo), '(Length, PTwo)]
  , dimDen ~ Density
  , dimZhz ~ '[ '(Time, NTwo)]

  , Convertible' dimLen uniLen
  , Convertible' dimPot uniPot
  , Convertible' dimDen uniDen
  , Convertible' dimZhz uniZhz
  , Convertible' dimZhz uniZhz'

  , MapMerge dimLen dimLen  dimLen2
  , MapNeg dimLen2  dimLenNeg2
  , MapMerge dimPot dimLenNeg2 dimZhz
  , MapMerge dimDen  '[ '(Time, NTwo), '(Length, PThree), '(Mass, NOne) ] dimZhz

  , MapMerge uniLen uniLen  uniLen2
  , MapNeg uniLen2  uniLenNeg2
  , MapMerge  uniPot uniLenNeg2 uniZhz
  , MapMerge uniDen   '[ '(Second, NTwo), '(Meter, PThree), '((Kilo Gram), NOne) ] uniZhz'

   ) =>

 (forall s. AD.Mode s =>
  Vec3 (Value dimLen uniLen (AD s x)) -> Value dimPot uniPot (AD s x))

  -> (Vec3 (Value dimLen uniLen x) -> (Value dimDen uniDen x))

  -> (Vec3 (Value dimLen uniLen x) -> (Value dimZhz uniZhz x))

gravityPoisson gravitationalPotential density r
  = laplacian gravitationalPotential r |-| (4 *| pi |*| density r |*| g)


gravitationalPotentialToDensity ::
  forall x
  dimLen dimDen dimDen' dimLen2 dimNegLen2 dimZhz dimNegGC dimPot
  uniLen uniDen uniDen' uniLen2 uniNegLen2 uniZhz uniNegGC uniPot .
  (Fractional x
  , dimLen ~ LengthDimension
  , dimPot ~ '[ '(Time, NTwo), '(Length, PTwo)]
  , dimDen ~ Density
  , MapEq dimDen' dimDen

  , Convertible' dimLen uniLen
  , Convertible' dimPot uniPot
  , Convertible' dimZhz uniZhz
  , Convertible' dimDen' uniDen'
  , Convertible' dimDen uniDen

  , MapMerge dimLen dimLen  dimLen2
  , MapNeg dimLen2  dimNegLen2
  , MapMerge dimPot dimNegLen2 dimZhz
  , MapNeg '[ '(Time, NTwo), '(Length, PThree), '(Mass, NOne) ] dimNegGC
  , dimNegGC ~  '[ '(Time, PTwo), '(Length, NThree), '(Mass, POne) ]
  , MapMerge dimZhz dimNegGC dimDen'

  , MapMerge uniLen uniLen  uniLen2
  , MapNeg uniLen2  uniNegLen2
  , MapMerge  uniPot uniNegLen2 uniZhz
  , MapNeg '[ '(Second, NTwo), '(Meter, PThree), '((Kilo Gram), NOne) ] uniNegGC
  , uniNegGC ~  '[ '(Second, PTwo), '(Meter, NThree), '((Kilo Gram), POne) ]
  , MapMerge uniZhz uniNegGC uniDen'

   ) =>
 (forall s. AD.Mode s =>
  Vec3 (Value dimLen uniLen (AD s x)) -> Value dimPot uniPot (AD s x))

  -> (Vec3 (Value dimLen uniLen x) -> (Value dimDen uniDen x))

gravitationalPotentialToDensity gravitationalPotential r
  =  to (undefined :: Value dimDen uniDen x) $
     (laplacian gravitationalPotential r |/| (4 *| pi |*| g) :: Value dimDen' uniDen' x)



hydrostatic ::
  forall x
  dimLen dimPre dimDen dimAcc dimGpr dimNegLen dimNegDen dimAcc'
  uniLen uniPre uniDen uniAcc uniGpr uniNegLen uniNegDen uniAcc'
  .
  ( Fractional x
  , dimLen ~ LengthDimension
  , dimPre ~ Pressure
  , dimDen ~ Density
  , dimAcc ~ Acceleration
  , dimNegDen ~ '[ '(Length, PThree),  '(Mass, NOne) ]
  , dimGpr ~  '[  '(Length, NTwo), '(Mass, POne) , '(Time, NTwo) ]

  , Convertible' dimLen uniLen
  , Convertible' dimPre uniPre
  , Convertible' dimGpr uniGpr
  , Convertible' dimDen uniDen
  , Convertible' dimAcc uniAcc
  , Convertible' dimAcc' uniAcc'

  , MapNeg   dimLen dimNegLen
  , MapMerge dimPre dimNegLen dimGpr
  , MapNeg   dimDen dimNegDen
  , MapMerge dimGpr dimNegDen dimAcc'

  , MapEq dimAcc' dimAcc

  , MapNeg   uniLen uniNegLen
  , MapMerge uniPre uniNegLen uniGpr
  , MapNeg   uniDen uniNegDen
  , MapMerge uniGpr uniNegDen uniAcc'

  ) =>

  (forall s. AD.Mode s =>
  Vec3 (Value dimLen uniLen (AD s x)) -> Value dimPre uniPre (AD s x))

  -> (Vec3 (Value dimLen uniLen x) -> Value dimDen uniDen x)

  -> (Vec3 (Value dimLen uniLen x) -> Vec3 (Value dimAcc uniAcc x))

  -> (Vec3 (Value dimLen uniLen x) ->  Vec3 (Value dimAcc uniAcc x))

hydrostatic pressure density externalAcc r
  = compose $ \i -> to (undefined :: Value dimAcc uniAcc x) $
    (externalAcc r ! i) |+| (gradP r ! i) |/| (density r)

  where
    gradP :: Vec3 (Value dimLen uniLen x) -> Vec3 (Value dimGpr uniGpr x)
    gradP = grad pressure



pressureToAcc ::
  forall x
  dimLen dimPre dimDen dimAcc dimGpr dimNegLen dimNegDen dimAcc'
  uniLen uniPre uniDen        uniGpr uniNegLen uniNegDen uniAcc'
  .
  ( Fractional x
  , dimLen ~ LengthDimension
  , dimPre ~ Pressure
  , dimDen ~ Density
  , dimAcc ~ Acceleration
  , dimNegDen ~ '[ '(Length, PThree),  '(Mass, NOne) ]
  , dimGpr ~  '[  '(Length, NTwo), '(Mass, POne) , '(Time, NTwo) ]

  , Convertible' dimLen uniLen
  , Convertible' dimPre uniPre
  , Convertible' dimGpr uniGpr
  , Convertible' dimDen uniDen
  , Convertible' dimAcc' uniAcc'
  , Convertible' dimAcc uniAcc'

  , MapNeg   dimLen dimNegLen
  , MapMerge dimPre dimNegLen dimGpr
  , MapNeg   dimDen dimNegDen
  , MapMerge dimGpr dimNegDen dimAcc'

  , MapEq dimAcc' dimAcc

  , MapNeg   uniLen uniNegLen
  , MapMerge uniPre uniNegLen uniGpr
  , MapNeg   uniDen uniNegDen
  , MapMerge uniGpr uniNegDen uniAcc'

  ) =>

  (forall s. AD.Mode s =>
  Vec3 (Value dimLen uniLen (AD s x)) -> Value dimPre uniPre (AD s x))

  -> (Vec3 (Value dimLen uniLen x) -> Value dimDen uniDen x)

  -> (Vec3 (Value dimLen uniLen x) ->  Vec3 (Value dimAcc uniAcc' x))

pressureToAcc pressure density r
  = compose $ \i -> to (undefined :: Value dimAcc uniAcc' x) $
    (gradP r ! i) |/| (density r)

  where
    gradP :: Vec3 (Value dimLen uniLen x) -> Vec3 (Value dimGpr uniGpr x)
    gradP = grad pressure

gravitationalPotentialToAcc ::
  forall x
  dimLen dimPot dimAcc' dimNegLen dimAcc
  uniLen uniPot uniAcc' uniNegLen
  .
  ( Fractional x
  , dimLen ~ LengthDimension
  , dimPot ~  '[ '(Time, NTwo), '(Length, PTwo)]
  , dimAcc' ~  '[ '(Length, POne), '(Time, NTwo)]
  , dimAcc ~ Acceleration


  , Convertible' dimLen uniLen
  , Convertible' dimPot uniPot
  , Convertible' dimAcc' uniAcc'
  , Convertible' dimAcc uniAcc'


  , MapNeg dimLen dimNegLen
  , MapMerge dimPot dimNegLen dimAcc'
  , MapEq dimAcc' dimAcc

  , MapNeg uniLen uniNegLen
  , MapMerge uniPot uniNegLen uniAcc'
   ) =>

  (forall s. AD.Mode s =>
   Vec3 (Value dimLen uniLen (AD s x)) -> Value dimPot uniPot (AD s x))

  -> (Vec3 (Value dimLen uniLen x) ->  Vec3 (Value dimAcc uniAcc' x))

gravitationalPotentialToAcc pot r =
  (fmap $ to (undefined :: Value dimAcc uniAcc x)) $
  grad pot r