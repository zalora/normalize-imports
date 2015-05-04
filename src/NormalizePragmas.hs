module SortPragmas (normalizePragmas) where

import           Data.Char (isSpace)
import           Data.List (isPrefixOf, sort)

import           Types

pragmaPrefix :: String
pragmaPrefix = "{-#"

shebangPrefix :: String
shebangPrefix = "#!"

normalizePragmas :: String -> String
normalizePragmas inp = unlines (sortPragmas header ++ rest)
  where
    (header, rest) = span isHeader (lines inp)

sortPragmas :: [String] -> [String]
sortPragmas = fromLines . map (fmap sort) . toLines isPragma

isPragma :: String -> Bool
isPragma = isPrefixOf pragmaPrefix

isEmptyLine :: String -> Bool
isEmptyLine s = null s || all isSpace s

isShebang :: String -> Bool
isShebang = isPrefixOf shebangPrefix

isHeader :: String -> Bool
isHeader s = isPragma s || isEmptyLine s || isShebang s
