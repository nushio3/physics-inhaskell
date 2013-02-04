module Text.CSL.Output.Haddock where

import Text.CSL (author, issued, Reference, title, url)
import Text.CSL.Reference(year)
import Text.Printf

citet :: Either String Reference -> String
citet eref =
  case eref of
    Left  _   -> "(??)"
    Right ref -> printf "(%s %s)" (unwords $ map show $ author ref) (year $ head $ issued ref)


citeUrl :: Either String Reference -> String
citeUrl eref =
  case eref of
    Left  _   -> "(??)"
    Right ref -> printf "%s (%s) '%s' <%s>" (unwords $ map show $ author ref) (year $ head $ issued ref)
                 (title ref) (url ref)