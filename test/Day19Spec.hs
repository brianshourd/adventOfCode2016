module Day19Spec (spec) where

import Day19
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "day19" $ do
        it "elf 3 wins in a circle of 5" $ do
            day19 5 `shouldBe` 3
        it "elf 9 wins in a circle of 20" $ do
            day19 20 `shouldBe` 9
        it "elf 1808357 wins in a circle of 3001330" $ do
            day19 3001330 `shouldBe` 1808357
    describe "day19'" $ do
        it "elf 2 wins in a circle of 5" $ do
            day19' 5 `shouldBe` 2
        it "elf 3 wins in a circle of 6" $ do
            day19' 6 `shouldBe` 3
        it "elf 1407007 wins in a circle of 3001330" $ do
            day19' 3001330 `shouldBe` 1407007
