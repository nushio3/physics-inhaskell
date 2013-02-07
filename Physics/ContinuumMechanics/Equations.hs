{- | We give equations by their left hand sides. -}

module Physics.ContinuumMechanics.Equations where

import Numeric.Physical.VectorCalculus


gravityPoisson gravitationalPotential density
  = gravitationalPotential - density