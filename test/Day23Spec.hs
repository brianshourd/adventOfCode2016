module Day23Spec (spec) where

import Day23
import Test.Hspec

main :: IO ()
main = hspec spec

sampleInput :: String
sampleInput = unlines
    [ "cpy 2 a"
    , "tgl a"
    , "tgl a"
    , "tgl a"
    , "cpy 1 a"
    , "dec a"
    , "dec a"
    ]

spec :: Spec
spec = do
    describe "day23" $ do
        it "processes sample input 1 to find 3 in register a" $ do
            day23 sampleInput `shouldBe` 3
        it "works on actual input" $ do
            actualInput <- readFile "inputs/day23.txt"
            day23 actualInput `shouldBe` 11340
    describe "day23'" $ do
        it "works on actual input" $ do
            actualInput <- readFile "inputs/day23.txt"
            day23 actualInput `shouldBe` 11340
