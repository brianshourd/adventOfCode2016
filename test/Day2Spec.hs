module Day2Spec (spec) where

import Day2
import Test.Hspec

main :: IO ()
main = hspec spec

sample :: String
sample = unlines
    [ "ULL"
    , "RRDDD"
    , "LURDL"
    , "UUUUD"
    ]

spec :: Spec
spec = do
    describe "day2" $ do
        it "gives the code '1985' for the provided sample" $ do
            day2 sample `shouldBe` "1985"
        it "gives the code '44558' for the actual input" $ do
            actualInput <- readFile "inputs/day2.txt"
            day2 actualInput `shouldBe` "44558"
    describe "day2'" $ do
        it "gives the code '5DB3' for the provided sample" $ do
            day2' sample `shouldBe` "5DB3"
        it "gives the code '6BBAD' for the actual input" $ do
            actualInput <- readFile "inputs/day2.txt"
            day2' actualInput `shouldBe` "6BBAD"
