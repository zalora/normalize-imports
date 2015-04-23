
module SortPragmasSpec where

import Test.Hspec

import SortPragmas

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "sortPragmas" $ do
    it "sorts multiple pragmas" $ do
      let input = [
              "{-# LANGUAGE Foo, Qux, Bar #-}"
            , "import Bla"
            ]
          output = [
              "{-# LANGUAGE Bar, Foo, Qux #-}"
            , "import Bla"
            ]
      sortPragmas (unlines input) `shouldBe` (unlines output)

    it "leaves single line pragmas unchanged" $ do
      let input = [
              "{-# LANGUAGE Foo #-}"
            , "import Bla"
            ]
          output = [
              "{-# LANGUAGE Foo #-}"
            , "import Bla"
            ]
      sortPragmas (unlines input) `shouldBe` (unlines output)
