{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Physics.NewtonMechanics.Concepts where

import           Data.Dynamic
import           Data.Object.Dynamic
import           Data.Object.Dynamic.Type
import           UnitTyped
import           UnitTyped.SI hiding (Mass)
import           UnitTyped.SI.Meta


data Mass = Mass deriving Typeable
instance (Objective o, UseReal o) => Member o Mass where
  type ValType o Mass = Value MassDimension (U (Kilo Gram)) (UnderlyingReal o)
