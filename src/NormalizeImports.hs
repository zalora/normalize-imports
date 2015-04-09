{-# LANGUAGE DeriveFunctor #-}

module NormalizeImports (normalizeImports) where

import Data.List
import Data.Char

importPrefix :: String
importPrefix = "import "

importQualified :: String
importQualified = "import qualified "

data Lines a = Verbatim [String] | Imports a
  deriving Functor

toLines :: [String] -> [Lines [String]]
toLines xs = case break isImportLine xs of
  (verbatim, ys) -> Verbatim verbatim : case span isImportLine ys of
      ([], []) -> []
      (imports, rest) -> Imports imports : toLines rest

fromLines :: [Lines [String]] -> [String]
fromLines = concatMap f
  where
    f :: Lines [String] -> [String]
    f xs = case xs of
      Verbatim ys -> ys
      Imports ys -> ys

isImportLine :: String -> Bool
isImportLine = isPrefixOf importPrefix

normalizeImports :: String -> String
normalizeImports = unlines . sortImports . map alignImport . lines

sortImports :: [String] -> [String]
sortImports = fromLines . map (fmap $ sortBy compareByModuleName) . toLines

compareByModuleName :: String -> String -> Ordering
compareByModuleName a b = compare (dropImport a) (dropImport b)
  where
    dropImport = drop (length importQualified)

alignImport :: String -> String
alignImport line
  | isImportLine line = aligned
  | otherwise = line
  where
    qualifiedPrefix = "qualified "

    modulePart :: String
    modulePart = (dropWhile isSpace . drop (length importPrefix)) line

    aligned
      | qualifiedPrefix `isPrefixOf` modulePart = importQualified ++ (dropWhile isSpace . drop (length qualifiedPrefix)) modulePart
      | otherwise = "import           " ++ modulePart
