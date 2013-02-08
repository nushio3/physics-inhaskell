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



hydrostatic ::
  forall x
  dimLen dimPre dimDen dimAcc dimGpr dimNegLen dimNegDen
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
  , Convertible' dimAcc uniAcc'

  , MapNeg   dimLen dimNegLen
  , MapMerge dimPre dimNegLen dimGpr
  , MapNeg   dimDen dimNegDen
  , MapMerge dimGpr dimNegDen dimAcc


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
  = compose $ \i -> (externalAcc r ! i) |+| (gradP r ! i) |/| (density r)

  where
    gradP :: Vec3 (Value dimLen uniLen x) -> Vec3 (Value dimGpr uniGpr x)
    gradP = grad pressure
