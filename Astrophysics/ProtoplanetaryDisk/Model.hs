{-# OPTIONS_GHC -F -pgmF embeddock -optF $ #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}


module Astrophysics.ProtoplanetaryDisk.Model
(
{- * Protoplanetary Disk models -}
bonnerEbertDensity, bonnerEbertPotential,

{- * References -}
{- $references -}

ebert1955, bonnor1956,
idaLin2003, ida2007

) where

import           System.IO.Unsafe(unsafePerformIO)
import           Text.CSL.Input.Identifier (readID, EReference)
import           Text.CSL.Output.Haddock (citet, citep, citeUrl)
import           UnitTyped
  (Value(..), (|/|), (|*|), (*|), as, square, val)
import           UnitTyped.Type
  (Convertible', MapInsert', MapMerge, MapStrip, MapNeg, MapEq, Nat(..), Number(..))
import qualified UnitTyped.Type as UT
import           UnitTyped.SI.Meta (Kilo)
import           UnitTyped.Type
                  (Value(..), U, val, mkVal,(|+|),(|-|), (|*|), (|/|), (*|), coerce,as,to, (:|),
                   Convertible'(..), MapMerge, MapNeg, MapEq, POne, PTwo, PThree,NOne, NTwo, NThree)

import           UnitTyped.SI (LengthDimension, Length, Mass, Time, Second, Meter, Gram)
import           UnitTyped.SI.Constants (g)
import           UnitTyped.SI.Derived (Speed, Density)

{- It is hard to type this, it's so long.... You can type this by
 type-inferring over e. g.:

bonnerEbertDensity cs r = square cs |/| (g |*| square r)
  where
    cs' = cs `as` knot
    r' = r `as` meter

-}
bonnerEbertDensity :: 
  ( Fractional f
  , dimLen ~ LengthDimension
  , dimDen ~ Density
  , dimSpd ~ Speed

--   , Convertible' dimSpd uniSpd
--   , Convertible' dimLen uniLen
--   , Convertible' dimDen uniDen
  , uniSpd ~ '[ '(Second, NOne), '(Meter, POne)]
  , uniDen ~ '[ '(Meter, NThree), '(Kilo Gram, POne)]
  , uniLen ~ '[ '(Meter, POne)]

 ) => Value dimSpd uniSpd f -> Value dimLen uniLen f -> Value dimDen uniDen f
bonnerEbertDensity cs r = square cs |/| (g |*| square r)


bonnerEbertPotential :: 
  ( Floating f
  , dimLen ~ LengthDimension
  , dimSpd ~ Speed
  , dimPot ~  '[ '(Time, NTwo), '(Length, PTwo)]

  , uniLen ~ '[ '(Meter, POne)]
  , uniSpd ~ '[ '(Second, NOne), '(Meter, POne)]
  , uniPot ~ '[ '(Second, NTwo), '(Meter, PTwo)]
  ) => Value dimSpd uniSpd f -> Value dimLen uniLen f -> Value dimPot uniPot f
bonnerEbertPotential cs r = (2 * log (val r)) *| square cs

{- ^
  Bonner-Ebert sphere is an hydrostatic solution of self-gravitating,
  isothermal gas, proposed by $(citet bonnor1956) and $(citet ebert1955) .
-}


{- $references

- $(citeUrl ebert1955)

- $(citeUrl bonnor1956)

- $(citeUrl idaLin2003)

- $(citeUrl ida2007)

-}


ebert1955, bonnor1956 :: EReference

ebert1955 = unsafePerformIO $ readID "bibcode:1955ZA.....37..217E"
bonnor1956 = unsafePerformIO $ readID "bibcode:1956MNRAS.116..351B"


idaLin2003 :: EReference
idaLin2003 = unsafePerformIO $ readID "arxiv:astro-ph/0312144"

ida2007 :: EReference
ida2007 = unsafePerformIO $ readID "isbn:978-4-13-060749-0"
