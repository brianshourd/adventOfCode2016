module Day11Spec (spec) where

import Day11
import Test.Hspec

main :: IO ()
main = hspec spec

sampleInput :: [String]
sampleInput =
    [ "The first floor contains a hydrogen-compatible microchip and a lithium-compatible microchip."
    , "The second floor contains a hydrogen generator."
    , "The third floor contains a lithium generator."
    , "The fourth floor contains nothing relevant."
    ]

sampleParsed :: State
sampleParsed = buildState 0
    [ Pair "hydrogen" 0 1
    , Pair "lithium" 0 2
    ]

spec :: Spec
spec = do
    describe "parseInput" $ do
        it "works on sample input" $ do
            parseInput (unlines sampleInput) `shouldBe` sampleParsed
    describe "day11" $ do
        it "works on actual input" $ do
            actualInput <- readFile "inputs/day11.txt"
            day11 actualInput `shouldBe` 37
    describe "day11'" $ do
        it "works on actual input" $ do
            actualInput <- readFile "inputs/day11.txt"
            day11' actualInput `shouldBe` 61
    describe "minPathLength" $ do
        it "works on sample input" $ do
            minPathLength (sampleParsed, desiredState sampleParsed) `shouldBe` 11
    describe "destroysChips" $ do
        it "sample state is valid" $ do
            destroysChips sampleParsed `shouldBe` False
        it "end state is valid" $ do
            destroysChips (desiredState sampleParsed) `shouldBe` False
        it "state with a microchip on the same floor as another generator is invalid" $ do
            let invalidState = buildState 0 [Pair "hydrogen" 0 1, Pair "lithium" 2 0]
            destroysChips invalidState `shouldBe` True
