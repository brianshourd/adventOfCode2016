module Day8Spec (spec) where

import Day8
import Test.Hspec

import Data.Array.Unboxed ((//), listArray)

main :: IO ()
main = hspec spec

sampleInput :: String
sampleInput = unlines
    [ "rect 3x2"
    , "rotate column x=1 by 1"
    , "rotate row y=0 by 4"
    , "rotate column x=1 by 1 "
    ]

sampleParsed :: [Instruction]
sampleParsed =
    [ Rect 3 2
    , RotateColumn 1 1
    , RotateRow 0 4
    , RotateColumn 1 1
    ]

-- Screens obtained, in order, by starting with empty and moving through the
-- sample instructions
sampleScreens :: [Screen]
sampleScreens =
    [ turnOn [(0, 0), (1, 0), (2, 0), (0, 1), (1, 1), (2, 1)]
    , turnOn [(0, 0), (2, 0), (0, 1), (1, 1), (2, 1), (1, 2)]
    , turnOn [(4, 0), (6, 0), (0, 1), (1, 1), (2, 1), (1, 2)]
    , turnOn [(1, 0), (4, 0), (6, 0), (0, 1), (2, 1), (1, 2)]
    ]

empty = listArray ((0, 0), (6, 2)) (repeat False)

turnOn :: [(Int, Int)] -> Screen
turnOn source = empty // [(i, True) | i <- source]

spec :: Spec
spec = do
    describe "day8" $ do
        it "finds 6 lit bulbs after following the sample instructions" $ do
            day8 sampleInput `shouldBe` 6
        it "finds 110 lit bulbs after following the actual instructions" $ do
            actualInput <- readFile "inputs/day8.txt"
            day8 actualInput `shouldBe` 110
    describe "parseInput" $ do
        it "works for sample input" $ do
            parseInput sampleInput `shouldBe` Right sampleParsed
    describe "applyInstructions"$ do
        it "works for sample input" $ do
            foldl applyInstruction empty sampleParsed `shouldBe` last sampleScreens
        it "works for rect" $ do
            applyInstruction empty (sampleParsed !! 0) `shouldBe` sampleScreens !! 0
        it "works for rotate column" $ do
            applyInstruction (sampleScreens !! 0) (sampleParsed !! 1) `shouldBe` sampleScreens !! 1
        it "works for rotate row" $ do
            applyInstruction (sampleScreens !! 1) (sampleParsed !! 2) `shouldBe` sampleScreens !! 2
        it "works for rotate column a second time" $ do
            applyInstruction (sampleScreens !! 2) (sampleParsed !! 3) `shouldBe` sampleScreens !! 3
