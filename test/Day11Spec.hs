module Day11Spec (spec) where

import Day11
import Test.Hspec

main :: IO ()
main = hspec spec

sampleInput :: String
sampleInput = unlines
    [
    ]

spec :: Spec
spec = do
    describe "day11" $ do
        it "works on actual input" $ do
            actualInput <- readFile "inputs/day11.txt"
            --day11 actualInput `shouldBe` 147
            pending
