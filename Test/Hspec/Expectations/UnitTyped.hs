module Test.Hspec.Expectations.UnitTyped (isOfOrderOf, isNegligible) where

import UnitTyped ()
import Test.HUnit.Lang (Assertion)

isOfOrderOf :: (Ord f, Fractional f,Convertible' a b, Convertible' c d, MapEq c a) => 
  Value a b f -> Value c d f -> Assertion
isOfOrderOf = undefined

isNegligible = 42