module Day15Spec (spec) where

import Day15
import Test.Hspec

main :: IO ()
main = hspec spec

sampleInput :: String
sampleInput = unlines
    [ "Disc #1 has 5 positions; at time=0, it is at position 4."
    , "Disc #2 has 2 positions; at time=0, it is at position 1."
    ]

sampleParsed :: [Disc]
sampleParsed = [ Disc 1 5 4, Disc 2 2 1 ]

actualParsed :: [Disc]
actualParsed =
    [ Disc 1 17 15
    , Disc 2 3 2
    , Disc 3 19 4
    , Disc 4 13 2
    , Disc 5 7 2
    , Disc 6 5 0
    ]

spec :: Spec
spec = do
    describe "parseInput" $ do
        it "parses sample input correctly" $ do
            parseInput sampleInput `shouldBe` sampleParsed
        it "parses actual input correctly" $ do
            actualInput <- readFile "inputs/day15.txt"
            parseInput actualInput `shouldBe` actualParsed
    describe "satisfy" $ do
        it "works on sample" $ do
            satisfy (map makeRequirement sampleParsed) `shouldBe` 5
    describe "day15" $ do
        it "determines to drop the ball at time t=5 for sample input" $ do
            day15 sampleInput `shouldBe` 5
        it "determines to drop the ball at time t=400589 for actual input" $ do
            actualInput <- readFile "inputs/day15.txt"
            day15 actualInput `shouldBe` 400589
    describe "day15'" $ do
        it "determines to drop the ball at time t=85 for sample input" $ do
            day15' sampleInput `shouldBe` 85
        it "determines to drop the ball at time t=3045959 for actual input" $ do
            actualInput <- readFile "inputs/day15.txt"
            day15' actualInput `shouldBe` 3045959
