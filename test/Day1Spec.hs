module Day1Spec (spec) where

import Day1
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "day1" $ do
        it "following `R2, L3` leaves you 5 blocks away" $ do
            day1 "R2, L3" `shouldBe` 5
        it "following `R2, R2, R2` leaves you 2 blocks away" $ do
            day1 "R2, R2, R2" `shouldBe` 2
        it "following `R5, L5, R5, R3` leaves you 12 blocks away" $ do
            day1 "R5, L5, R5, R3" `shouldBe` 12
        it "following the actual input leaves you 262 blocks away" $ do
            actualInput <- readFile "inputs/day1.txt"
            day1 actualInput `shouldBe` 262
    describe "day1'" $ do
        it "following `R8, R4, R4, R8` hits the same location 4 blocks away" $ do
            day1' "R8, R4, R4, R8" `shouldBe` 4
        it "following the actual input hits the same location 131 blocks away" $ do
            actualInput <- readFile "inputs/day1.txt"
            day1' actualInput `shouldBe` 131
