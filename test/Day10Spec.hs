module Day10Spec (spec) where

import Day10
import Test.Hspec

main :: IO ()
main = hspec spec

sampleInput :: String
sampleInput = unlines
    [ "value 5 goes to bot 2"
    , "bot 2 gives low to bot 1 and high to bot 0"
    , "value 3 goes to bot 1"
    , "bot 1 gives low to output 1 and high to bot 0"
    , "bot 0 gives low to output 2 and high to output 0"
    , "value 2 goes to bot 2"
    ]

sampleParsed :: [Expr]
sampleParsed =
    [ SourceExpr (Chip 5) (RobotDest 2)
    , BotExpr 2 (RobotDest 1) (RobotDest 0)
    , SourceExpr (Chip 3) (RobotDest 1)
    , BotExpr 1 (OutputDest 1) (RobotDest 0)
    , BotExpr 0 (OutputDest 2) (OutputDest 0)
    , SourceExpr (Chip 2) (RobotDest 2)
    ]

spec :: Spec
spec = do
    describe "parseInput" $ do
        it "parses sample input correctly" $ do
            parseInput sampleInput `shouldBe` sampleParsed
    describe "detectBot" $ do
        it "detects that bot 2 is responsible for comparing value-5 and value-2 microchips on sample input" $ do
            detectBot sampleParsed (Chip 5, Chip 2) `shouldBe` 2
        it "works even when chip order is reversed" $ do
            detectBot sampleParsed (Chip 2, Chip 5) `shouldBe` 2
    describe "day10" $ do
        it "detects that bot 147 is responsible for comparing value-61 and value-17 microchips on actual input" $ do
            actualInput <- readFile "inputs/day10.txt"
            day10 actualInput `shouldBe` 147
    describe "day10'" $ do
        it "detects that the product of outputs 0, 1, and 2 is 30 in sample input" $ do
            day10' sampleInput `shouldBe` 30
        it "detects that the product of outputs 0, 1, and 2 is 55637 in actual input" $ do
            actualInput <- readFile "inputs/day10.txt"
            day10' actualInput `shouldBe` 55637
