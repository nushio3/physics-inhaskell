{-# OPTIONS_GHC -F -pgmF embeddock -optF $ #-}
{-# LANGUAGE DataKinds #-}
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
import           UnitTyped.SI (Gram, Mass, Length, LengthDimension, Meter, Time, Second, meter)
import           UnitTyped.SI.Constants (g)
import           UnitTyped.SI.Derived (Speed, Density, knot)

{- It is hard to type this, it's so long.... You can type this by
 type-inferring over e. g.:

bonnerEbertDensity cs r = square cs |/| (g |*| square r)
  where
    cs' = cs `as` knot
    r' = r `as` meter

-}
bonnerEbertDensity :: (Fractional f, MapInsert' () Mass (Neg One) rest3 inserted, MapInsert' () Length (Pos (Suc (Suc One))) rest1 inserted1, MapInsert' () Time (Neg (Suc One)) rest2 inserted2, MapInsert' () (Kilo Gram) (Neg One) rest6 inserted3, MapInsert' () Meter (Pos (Suc (Suc One))) rest5 inserted4, MapInsert' () Second (Neg (Suc One)) rest4 inserted5, MapStrip inserted rest1, MapStrip inserted1 rest2, MapStrip inserted2 c, MapStrip inserted3 rest5, MapStrip inserted4 rest4, MapStrip inserted5 d, MapMerge a c' u, MapMerge b d' s, MapMerge c1 c1 a, MapMerge d1 d1 b, MapMerge c2 c2 rest3, MapMerge d2 d2 rest6, MapNeg c c', MapNeg d d', MapEq c2 LengthDimension, MapEq c1 Speed, Convertible' c2 d2, Convertible' c1 d1, Convertible' a b, Convertible' c d, Convertible' rest3 rest6) => Value c1 d1 f -> Value c2 d2 f -> Value u s f
bonnerEbertDensity cs r = square cs |/| (g |*| square r)


bonnerEbertPotential :: (Floating f, MapMerge c c a, MapMerge d d b, MapEq a1 LengthDimension, MapEq c Speed, Convertible' a1 b1, Convertible' c d, Convertible' a b) => Value c d f -> Value a1 b1 f -> Value a b f
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
