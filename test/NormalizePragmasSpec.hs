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
      normalizePragmas (unlines input) `shouldBe` unlines output

    it "preserves spaces inside pragmas" $ do
      let input = ["{-#  LANGUAGE A    #-}"]
      normalizePragmas (unlines input) `shouldBe` unlines input

    it "preserves spaces between pragmas" $ do
      let input = [
              "{-# LANGUAGE B #-}"
            , ""
            , "{-# LANGUAGE A #-}"
            ]
      normalizePragmas (unlines input) `shouldBe` unlines input

    it "allows shebang before pragmas" $ do
      let input = [
              "#!/usr/bin/env something"
            , ""
            , "{-# LANGUAGE A #-}"
            ]
      normalizePragmas (unlines input) `shouldBe` unlines input
