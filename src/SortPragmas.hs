module SortPragmas (sortPragmas) where

import           Data.List (isPrefixOf, sort)

pragmaPrefix :: String
pragmaPrefix = "{-#"

sortPragmas :: String -> String
sortPragmas inp = unlines (sort pragmas ++ rest)
  where
    (pragmas, rest) = span isPragma (lines inp)

isPragma :: String -> Bool
isPragma = isPrefixOf pragmaPrefix
