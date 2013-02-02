{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Physics.NewtonMechanics.Concepts where

import           Data.Dynamic
import           Data.Object.Dynamic.Type
  (acyclically, its, Member(type ValType, memberLookup), Objective )
import           Data.Object.Dynamic.Underlying(UnderlyingReal, UseReal)
import           UnitTyped
import           UnitTyped.SI hiding (Mass)
import           UnitTyped.SI.Meta
import qualified UnitTyped.SI.Derived as Dim






-- | The concept of 'Mass' of an object.
data Mass = Mass deriving Typeable

instance (Objective o, UseReal o)
         => Member o Mass where
  type ValType o Mass =
    Value
    MassDimension
    (U (Kilo Gram))
    (UnderlyingReal o)


-- | The concept of 'Speed' of an object.
data Speed = Speed deriving Typeable

instance (Objective o, UseReal o, Fractional (UnderlyingReal o))
         => Member o Speed where

  type ValType o Speed =
    Value Dim.Speed
    '[ '(Second, NOne), '(Meter, POne)]
    (UnderlyingReal o)

  memberLookup = acyclically $ do
    m  <- its Mass
    mv <- its Momentum
    return $ mv |/| m `as` undefined -- invoke type-inference assisted conversion!

-- | The concept of 'Momentum' of an object.
data Momentum = Momentum deriving Typeable

instance (Objective o, UseReal o, Fractional (UnderlyingReal o))
         => Member o Momentum where
  type ValType o Momentum =
    Value
    Dim.Momentum
    '[ '(Second, NOne), '(Meter, POne), '((Kilo Gram), POne)]
    (UnderlyingReal o)

  memberLookup = acyclically $ do
    m <- its Mass
    v <- its Speed
    return $ m |*| v `as` undefined -- invoke type-inference assisted conversion!

-- | The concept of 'KineticEnergy' of an object.
--  $(citet ida2007)

data KineticEnergy = KineticEnergy deriving Typeable

instance (Objective o, UseReal o, Fractional (UnderlyingReal o))
         => Member o KineticEnergy where
  type ValType o KineticEnergy =
    Value
    Dim.Energy
    (U Dim.Joule)
    (UnderlyingReal o)

  memberLookup = acyclically $ do
    m <- its Mass
    v <- its Speed
    return $ m |*| v |*| v `as` undefined -- invoke type-inference assisted conversion!


