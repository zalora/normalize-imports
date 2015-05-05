module NormalizePragmas (normalizePragmas) where

import           Data.List (isPrefixOf, sort)

import           Types

normalizePragmas :: String -> String
normalizePragmas = unlines . sortPragmas . lines

sortPragmas :: [String] -> [String]
sortPragmas = fromLines . map (fmap sort) . toLines isPragma

isPragma :: String -> Bool
isPragma s = any (`isPrefixOf` s) ["{-# LANGUAGE", "{-# OPTIONS_GHC"]
