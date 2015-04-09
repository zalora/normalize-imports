
module NormalizeImportsSpec where

import Test.Hspec

import NormalizeImports

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "normalizeImport" $ do
    it "aligns imports" $ do
      let input = [
              "import Bar"
            , "import  qualified  Foo"
            ]
          output = [
              "import           Bar"
            , "import qualified Foo"
            ]
      normalizeImports (unlines input) `shouldBe` (unlines output)

    it "sorts imports" $ do
      let input = [
              "import           Foo"
            , "import           Bar"
            , "foo = 23"
            ]
          output = [
              "import           Bar"
            , "import           Foo"
            , "foo = 23"
            ]
      normalizeImports (unlines input) `shouldBe` (unlines output)

    context "when there are multiple import blocks" $ do
      it "sorts each import block independently" $ do
        let input = [
                "import           Foo"
              , "import           Bar"
              , "-- comment"
              , "import           Foo"
              , "import           Baz"
              ]
            output = [
                "import           Bar"
              , "import           Foo"
              , "-- comment"
              , "import           Baz"
              , "import           Foo"
              ]
        normalizeImports (unlines input) `shouldBe` (unlines output)

    context "with qualified imports" $ do
      it "sorts by module names" $ do
        let input = [
                "import           Foo"
              , "import qualified Bar"
              ]
            output = [
                "import qualified Bar"
              , "import           Foo"
              ]
        normalizeImports (unlines input) `shouldBe` (unlines output)
