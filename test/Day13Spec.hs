module Day13Spec (spec) where

import Day13
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "stepsToReach" $ do
        it "takes 11 steps to reach (7, 4) with seed 10" $ do
            stepsToReach 10 (7, 4) `shouldBe` 11
    describe "day13" $ do
        it "takes 86 steps to reach (31, 39) with seed 1364" $ do
            day13 1364 `shouldBe` 86
    describe "numLocationsReached" $ do
        it "can reach 5 locations in 2 steps with seed 10" $ do
            numLocationsReached 10 2 `shouldBe` 5
    describe "day13'" $ do
        it "can reach 127 locations in 50 steps with seed 1364" $ do
            day13' 1364 `shouldBe` 127
