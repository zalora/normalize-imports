module NormalizePragmasSpec where

import           Test.Hspec

import           NormalizePragmas

spec :: Spec
spec = do
  describe "sortPragmas" $ do
    it "sort pragmas" $ do
      let input = [
              "{-# LANGUAGE B #-}"
            , "{-# LANGUAGE Ab #-}"
            , "{-# OPTIONS_GHC Abc #-}"
            , "{-# LANGUAGE A #-}"
            ]
          output = [
              "{-# LANGUAGE A #-}"
            , "{-# LANGUAGE Ab #-}"
            , "{-# LANGUAGE B #-}"
            , "{-# OPTIONS_GHC Abc #-}"
            ]
      normalizePragmas (unlines input) `shouldBe` unlines output

    it "preserves spaces inside pragmas" $ do
      let input = ["{-#  LANGUAGE A    #-}"]
      normalizePragmas (unlines input) `shouldBe` unlines input

    it "preserves lines that are not language pragmas or ghc options" $ do
      let input = [
              "{- First line }-"
            , "{-# LANGUAGE B #-}"
            , "{- Here we have"
            , "a multiple line comment -}"
            , "{-# LANGUAGE A #-}"
            , "{-# INCLUDE <fp.h> #-}"
            ]
      normalizePragmas (unlines input) `shouldBe` unlines input
