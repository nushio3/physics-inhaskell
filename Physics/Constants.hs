{-# OPTIONS_GHC -F -pgmF embeddock -optF $$ #-}

module Physics.Constants where

import UnitTyped
import UnitTyped.SI
import UnitTyped.SI.Meta

{- | mass of $$(nameOfStar) is $$(show solarMass)

>>> 1+1
2
-}

solarMass :: Value MassDimension (U Gram) Double
solarMass =  3e30 *| kilo gram `as` gram

nameOfStar = "Sun"