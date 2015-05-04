
module Main where

import           NormalizeImports
import           SortPragmas

main :: IO ()
main = interact (normalizePragmas . normalizeImports)
