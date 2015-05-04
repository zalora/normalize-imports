module SortPragmasSpec where

import           Test.Hspec

import           SortPragmas

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
      sortPragmas (unlines input) `shouldBe` unlines output

    it "preserves spaces in pragma" $ do
      let input = ["{-#  LANGUAGE A    #-}"]
      sortPragmas (unlines input) `shouldBe` unlines input
