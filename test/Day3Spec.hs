module Day3Spec (spec) where

import Day3
import Test.Hspec

main :: IO ()
main = hspec spec

sampleInput :: String
sampleInput = unlines
    [ "   5  10  25"
    , "  10  10   1"
    , "   3   4   5"
    , "  15  10   6"
    , "   3   3   3"
    , "   5 100 101"
    ]

spec :: Spec
spec = do
    describe "day3" $ do
        it "finds 5 valid triangles horizontally in sample input" $ do
            day3 sampleInput `shouldBe` 5
        it "finds 982 valid triangles horizontally in actual input" $ do
            actualInput <- readFile "inputs/day3.txt"
            day3 actualInput `shouldBe` 982
    describe "day3'" $ do
        it "finds 1 valid triangle vertically" $ do
            day3' sampleInput `shouldBe` 1
        it "finds 1826 valid triangles vertically in actual input" $ do
            actualInput <- readFile "inputs/day3.txt"
            day3' actualInput `shouldBe` 1826
