module Astrophysics.ProtoplanetaryDisk.ModelSpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck (prop)


spec :: Spec
spec = describe "Bonner Ebert Sphere" $ do
  it "is a sphere" $ do
     1==2