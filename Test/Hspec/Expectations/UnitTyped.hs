module Test.Hspec.Expectations.UnitTyped (isOfOrderOf, isNegligible) where

import Prelude hiding (abs)
import UnitTyped.Type (Convertible', MapEq, Value(..), (|<|),(|>|), (*|))
import UnitTyped.NoPrelude (abs)
import Test.HUnit.Lang (Assertion, assertFailure)


isOfOrderOf :: (Ord f, Show f, Fractional f,Convertible' a b, Convertible' c d, MapEq c a) => 
  Value a b f -> Value c d f -> Assertion
isOfOrderOf x y 
  | x' |<| 10 *| y' && x' |>| 0.1 *| y' = return ()
  | otherwise                           = assertFailure $
    "expected " ++ show x ++ " to be of order of " ++ show y 
                ++ ", but it isn't."
  where
    x' = abs x
    y' = abs y

isNegligible  :: (Ord f, Show f, Fractional f,Convertible' a b, Convertible' c d, MapEq c a) => 
  f -> Value a b f -> Value c d f -> Assertion
isNegligible epsilon x y 
  | abs x |<| epsilon *| abs y = return ()
  | otherwise                  = assertFailure $
    "expected " ++ show x ++ " to be smaller than " ++ show epsilon ++ " * " ++ show y 
                ++ ", but it isn't."
