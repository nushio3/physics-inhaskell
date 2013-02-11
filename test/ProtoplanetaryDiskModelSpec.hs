{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module ProtoplanetaryDiskModelSpec (spec) where

import Astrophysics.ProtoplanetaryDisk.Model (bonnerEbertDensity, bonnerEbertPotential, bonnor1956, ebert1955)

import Data.Foldable (foldl1)
import Data.Tensor.TypeLevel (vec3, Vec3)

import           Numeric.Physical.VectorCalculus (grad)

import Physics.ContinuumMechanics.Equations 

import Prelude hiding (abs, sqrt, foldl1)

import System.IO
import System.IO.Unsafe (unsafePerformIO)


import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.Hspec.Expectations.UnitTyped (isOfOrderOf)
import Test.HUnit.Lang (Assertion, performTestCase)
import Test.QuickCheck ((==>), Arbitrary, Property)
import Test.QuickCheck.Property (morallyDubiousIOProperty)

import           Text.CSL.Output.Haddock (citet, citep, citeUrl)

import UnitTyped (
       Value(..), NOne, POne, NTwo, PTwo, NThree, U,
       to, per,(|+|), (|-|), (*|), (|*|), (|/|), (|>=|),(|<=|),(|>|),(|==|), square, (:|), mkVal )
import UnitTyped.SI (Gram, Meter, Length, Second, Time, gram, kelvin, meter, second)
import UnitTyped.SI.Constants (kB, m_p)
import UnitTyped.SI.Derived (Speed, Density, Pressure, Pascal,Acceleration, pascal)
import UnitTyped.SI.Derived.Length (astronomicalUnit, lightYear, parsec, AstronomicalUnit)
import           UnitTyped.SI.Meta (Kilo)             
import UnitTyped.NoPrelude (abs, sqrt)


execAss :: Assertion -> Property
execAss ass = morallyDubiousIOProperty $ (fmap (==Nothing)) $ performTestCase ass

printe :: Show a => a -> IO ()
printe = hPutStrLn stderr . show

spec :: Spec
spec = describe ("Bonner Ebert Sphere by " ++  citet bonnor1956  ++ " & " ++ citet ebert1955) $ do
  it "temperature is 10K" $ do
     soundSpeed `isOfOrderOf` (100 *| meter |/| second)
  prop "radius is positive" $ 
     \xyz ->  xyzToR xyz |>=| (0::Double) *| astronomicalUnit

  prop "has correct gravitational potential 2" $
     \xyz -> 
        let 
            den' :: Vec3 (Double :| Meter) -> Value Density '[ '(Meter, NThree), '(Kilo Gram, POne)] Double
            den' = gravitationalPotentialToDensity (bonnerEbertPotential soundSpeed . xyzToR)
            den = bonnerEbertDensity soundSpeed . xyzToR
        in
          xyzToR xyz |>| (0 *| meter)  ==> 
            abs(den' xyz |-| den xyz)  |<=| 1e-10 *| den xyz

  prop "is hydrostatic" $ 
     \xyz -> 
        let pre :: Floating f => Vec3 (f :| Meter) -> Value Pressure (U Pascal) f 
            pre = (to pascal) .(|*| square soundSpeed) . bonnerEbertDensity soundSpeed . xyzToR
            acc :: Floating f => Vec3 (f :| Meter) -> 
                Vec3 (Value Acceleration  '[ '(Second, NTwo), '(Meter, POne)] f )
            acc = (fmap $ to (meter `per` square second)) . grad (bonnerEbertPotential soundSpeed . xyzToR)
            den :: Vec3 (Double :| Meter) -> Value Density '[ '(Meter, NThree), '(Kilo Gram, POne)] Double
            den = bonnerEbertDensity soundSpeed . xyzToR
            epsilon :: Value Acceleration '[ '(Second, NTwo), '(Meter, POne) ] Double
            epsilon = mkVal 1e-3

            h = hydrostatic pre den acc xyz
        in 
          xyzToR xyz |>| (0 *| meter)  ==> 
             epsilon  |<=| epsilon
                



soundSpeed :: Floating f => Value Speed  '[   '(Second, NOne), '(Meter, POne) ] f
soundSpeed =
  sqrt $
  to (square meter `per` square second) $ (1.4 *| kB |*| (10 *| kelvin) ) |/| (2 *| m_p)

xyzToR :: Floating f => Vec3 (f :| Meter) -> (f :| Meter) 
xyzToR xyz = 
  sqrt $ foldl1 (|+|) $ fmap square xyz