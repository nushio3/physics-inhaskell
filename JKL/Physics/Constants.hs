module JKL.Physics.Constants where

import UnitTyped
import UnitTyped.SI
import UnitTyped.SI.Meta

solarMass :: Value MassDimension (U Gram) Double
solarMass =  3e30 *| kilo gram `as` gram
