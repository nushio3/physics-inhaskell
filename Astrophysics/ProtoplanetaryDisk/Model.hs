{-# OPTIONS_GHC -F -pgmF embeddock -optF $ #-}

module Astrophysics.ProtoplanetaryDisk.Model
(
{- * Protoplanetary Disk models -}
mass,

{- * References -}
ida2007, idaLin2003

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



{- |
$(citeUrl ida2007)
-}

ida2007 :: Either String Reference
ida2007 = unsafePerformIO $ readID "isbn:978-4-13-060749-0"

{- |
$(citeUrl idaLin2003)
-}

idaLin2003 :: Either String Reference
idaLin2003 = unsafePerformIO $ readID "arxiv:astro-ph/0312144"