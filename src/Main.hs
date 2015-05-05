
module Main where

import           NormalizeImports
import           NormalizePragmas

main :: IO ()
main = interact (normalizePragmas . normalizeImports)
