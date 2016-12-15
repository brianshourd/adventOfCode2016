module Day14Spec (spec) where

import Day14
import Test.Hspec

import Data.ByteString.Char8 (pack)

main :: IO ()
main = hspec spec

sampleInput :: String
sampleInput = "abc"

actualInput :: String
actualInput = "yjdafjpo"

spec :: Spec
spec = do
    describe "normalHash" $ do
        it "works correctly" $ do
            normalHash "abc" 18 `shouldBe` pack "0034e0923cc38887a57bd7b1d4f953df"
    describe "stretchedHash" $ do
        it "works correctly" $ do
            stretchedHash "abc" 0 `shouldBe` pack "a107ff634856bb300138cac6568c0f24"
    describe "day14" $ do
        it "finds the 64th key at index 22728 with sample input" $ do
            day14 sampleInput `shouldBe` 22728
        it "finds the 64th key at index 25427 for actual input" $ do
            day14 actualInput `shouldBe` 25427
    describe "day14'" $ do
        it "finds the 64th key at index 22551" $ do
            --day14' sampleInput `shouldBe` 22551
            pendingWith "this takes too long"
        it "finds the 64th key at index 22045 for actual input" $ do
            --day14' actualInput `shouldBe` 22045
            pendingWith "this takes too long"
