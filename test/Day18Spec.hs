module Day18Spec (spec) where

import Day18
import Test.Hspec

main :: IO ()
main = hspec spec

sampleInput1 :: Floor
sampleInput1 = buildFloor 3 "..^^."

sampleInput2 :: Floor
sampleInput2 = buildFloor 10 ".^^.^.^^^^"

spec :: Spec
spec = do
    describe "countSafeTiles" $ do
        it "finds 6 safe tiles in the sample 5x3 input" $ do
            countSafeTiles sampleInput1 `shouldBe` 6
        it "finds 38 safe tiles in the sample 10x10 input" $ do
            countSafeTiles sampleInput2 `shouldBe` 38
    describe "day18" $ do
        it "finds 1982 safe tiles in actual input" $ do
            actualInput <- readFile "inputs/day18.txt"
            day18 actualInput `shouldBe` 1982
    describe "day18'" $ do
        it "finds 20005203 safe tiles in actual input" $ do
            actualInput <- readFile "inputs/day18.txt"
            --day18' actualInput `shouldBe` 20005203
            pendingWith "too slow"
