{-# OPTIONS_GHC -F -pgmF embeddock -optF $ #-}

module Astrophysics.ProtoplanetaryDisk.Model
(
{- * Protoplanetary Disk models -}
mass,

{- * References -}
{- $references -}

) where

import           System.IO.Unsafe(unsafePerformIO)
import           Text.CSL(Reference)
import           Text.CSL.Input.Identifier (readID)
import           Text.CSL.Output.Haddock(citet, citeUrl)


{- |
 Typical mass for ppd.
 $(citet ida2007)
-}
mass :: Double
mass = 42


{- $references

- $(citeUrl ida2007)

- $(citeUrl idaLin2003)

-}


ida2007 :: Either String Reference
ida2007 = unsafePerformIO $ readID "isbn:978-4-13-060749-0"

idaLin2003 :: Either String Reference
idaLin2003 = unsafePerformIO $ readID "arxiv:astro-ph/0312144"
