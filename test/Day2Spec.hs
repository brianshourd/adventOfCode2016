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
        it "works for the provided sample" $ do
            day2 sample `shouldBe` "1985"
    describe "day2'" $ do
        it "works for the provided sample" $ do
            day2' sample `shouldBe` "5DB3"
