module Day5Spec (spec) where

import Day5
import Test.Hspec

main :: IO ()
main = hspec spec

sampleDoorId :: String
sampleDoorId = "abc"

spec :: Spec
spec = do
    describe "day5" $ do
        it "calculates a password of '18f47a30'" $ do
            --day5 sampleDoorId `shouldBe` "18f47a30"
            pendingWith "this spec takes too long"
    describe "day5'" $ do
        it "calculates a password of '05ace8e3'" $ do
            --day5' sampleDoorId `shouldBe` "05ace8e3"
            pendingWith "this spec takes too long"
    describe "startsWithZeros" $ do
        it "returns true for hash of abc3231929" $ do
            startsWithZeros (applyHash "abc" 3231929) `shouldBe` True
        it "returns true for hash of abc5017308" $ do
            startsWithZeros (applyHash "abc" 5017308) `shouldBe` True
