module Day20Spec (spec) where

import Day20
import Test.Hspec

main :: IO ()
main = hspec spec

sampleInput :: String
sampleInput = unlines ["5-8", "0-2", "4-7"]

sampleParsed :: [Range]
sampleParsed = [(5,  8), (0, 2), (4, 7)]

spec :: Spec
spec = do
    describe "parseInput" $ do
        it "works for sample input" $ do
            parseInput sampleInput `shouldBe` sampleParsed
    describe "firstValid" $ do
        it "works for sample input" $ do
            firstValid sampleParsed `shouldBe` 3
    describe "day20" $ do
        it "works for sample input" $ do
            day20 sampleInput `shouldBe` 3
        it "works for actual input" $ do
            actualInput <- readFile "inputs/day20.txt"
            day20 actualInput `shouldBe` 23923783
    describe "countValid" $ do
        it "works for sample input" $ do
            countValid 9 sampleParsed `shouldBe` 2
    describe "day20'" $ do
        it "works for actual input" $ do
            actualInput <- readFile "inputs/day20.txt"
            day20' actualInput `shouldBe` 125
