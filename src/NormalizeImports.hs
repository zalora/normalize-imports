module NormalizeImports (normalizeImports) where

import           Data.Char
import           Data.List

import           Types

importPrefix :: String
importPrefix = "import "

importQualified :: String
importQualified = "import qualified "

isImportLine :: String -> Bool
isImportLine = isPrefixOf importPrefix

normalizeImports :: String -> String
normalizeImports = unlines . sortImports . map alignImport . lines

sortImports :: [String] -> [String]
sortImports = fromLines . map (fmap $ sortBy compareByModuleName) . toLines isImportLine

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
