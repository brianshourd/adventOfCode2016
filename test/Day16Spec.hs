module Day16Spec (spec) where

import Day16
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "zeroDragon" $ do
        it "should begin like '001001100011011'" $ do
            toString (take 15 zeroDragon) `shouldBe` "001001100011011"
    describe "day16" $ do
        it "finds a checksum of '01100' with starting state '10000' for disk length 20" $ do
            day16 "10000" 20 `shouldBe` "01100"
        it "finds a checksum of '10101001010100001' with actual starting state for disk length 272" $ do
            day16 "10001001100000001" 272 `shouldBe` "10101001010100001"
