module Day6Spec (spec) where

import Day6
import Test.Hspec

main :: IO ()
main = hspec spec

sampleInput :: String
sampleInput = unlines
    [ "eedadn"
    , "drvtee"
    , "eandsr"
    , "raavrd"
    , "atevrs"
    , "tsrnev"
    , "sdttsa"
    , "rasrtv"
    , "nssdts"
    , "ntnada"
    , "svetve"
    , "tesnvt"
    , "vntsnd"
    , "vrdear"
    , "dvrsen"
    , "enarar"
    ]

spec :: Spec
spec = do
    describe "day6" $ do
        it "decodes sample input as 'easter'" $ do
            day6 sampleInput `shouldBe` "easter"
        it "decodes actual input as 'ygjzvzib'" $ do
            actualInput <- readFile "inputs/day6.txt"
            day6 actualInput `shouldBe` "ygjzvzib"
    describe "day6'" $ do
        it "decodes sample input as 'advent'" $ do
            day6' sampleInput `shouldBe` "advent"
        it "decodes actual input as 'pdesmnoz'" $ do
            actualInput <- readFile "inputs/day6.txt"
            day6' actualInput `shouldBe` "pdesmnoz"

