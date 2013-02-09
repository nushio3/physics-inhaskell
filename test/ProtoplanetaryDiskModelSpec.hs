{-# LANGUAGE DataKinds #-}

module ProtoplanetaryDiskModelSpec (spec) where

import Astrophysics.ProtoplanetaryDisk.Model (bonnerEbertDensity, bonnerEbertPotential)

import Prelude hiding (sqrt)

import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.Hspec.Expectations.UnitTyped (isOfOrderOf)

import UnitTyped (Value(..), NOne, POne, to, per, (*|), (|*|), (|/|), square )
import UnitTyped.SI (Meter, Second, gram, kelvin, meter, second)
import UnitTyped.SI.Constants (kB, m_p)
import UnitTyped.SI.Derived (Speed)
import UnitTyped.SI.Derived.Length (astronomicalUnit, lightYear, parsec)
import UnitTyped.NoPrelude (sqrt)


spec :: Spec
spec = describe "Bonner Ebert Sphere" $ do
  it "temperature is 10K" $ do
     soundSpeed `isOfOrderOf` (100 *| meter |/| second)





soundSpeed :: Floating f => Value Speed  '[   '(Second, NOne), '(Meter, POne) ] f
soundSpeed =
  sqrt $
  to (square meter `per` square second) $ (1.4 *| kB |*| (10 *| kelvin) ) |/| (2 *| m_p)

