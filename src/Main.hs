
module Main where

import NormalizeImports

main :: IO ()
main = interact normalizeImports
