{-# LANGUAGE DeriveFunctor #-}

module Types where

data Lines a = Verbatim [String] | Sorts a
  deriving Functor

toLines :: (String -> Bool) -> [String] -> [Lines [String]]
toLines predicate xs = case break predicate xs of
  (verbatim, ys) -> Verbatim verbatim : case span predicate ys of
      ([], []) -> []
      (sorts, rest) -> Sorts sorts : toLines predicate rest

fromLines :: [Lines [String]] -> [String]
fromLines = concatMap f
  where
    f :: Lines [String] -> [String]
    f xs = case xs of
      Verbatim ys -> ys
      Sorts ys -> ys
