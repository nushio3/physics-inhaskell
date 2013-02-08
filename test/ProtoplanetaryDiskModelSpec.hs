module ProtoplanetaryDiskModelSpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck (prop)

import UnitTyped.SI.Derived.Length (lightYear, parsec)

spec :: Spec
spec = describe "Bonner Ebert Sphere" $ do
  it "is a sphere" $ do
     True


