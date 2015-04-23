
module SortPragmas where

import           Data.Char
import           Data.List

pragmaPrefix :: String
pragmaPrefix = "{-# LANGUAGE "

sortPragmas :: String -> String
sortPragmas = unlines . sortIfPragma . lines
  where
    sortIfPragma :: [String] -> [String]
    sortIfPragma [] = []
    sortIfPragma (x:xs)
      | pragmaPrefix `isPrefixOf` x = sorted x : sortIfPragma xs
      | otherwise = x : sortIfPragma xs

    sorted :: String -> String
    sorted s = pragmaPrefix ++ (sortedPragmas s) ++ " #-}"
      where
        onlyPragmas = splitToWords . takeWhile (/= '#') .  drop (length pragmaPrefix)
        sortedPragmas = (intercalate ", ") . sort . onlyPragmas

splitToWords :: String -> [String]
splitToWords xs =
  case dropWhile p xs of
      "" -> []
      s -> w' : splitToWords s2
        where
          (w, s2) = break p s
          w' = dropWhile isSpace $ dropWhileEnd isSpace w
  where
    p = (== ',')
