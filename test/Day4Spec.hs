module Day4Spec (spec) where

import Day4
import Test.Hspec

main :: IO ()
main = hspec spec

sampleInputs :: [String]
sampleInputs =
    [ "aaaaa-bbb-z-y-x-123[abxyz]"
    , "a-b-c-d-e-f-g-h-987[abcde]"
    , "not-a-real-room-404[oarel]"
    , "totally-real-room-200[decoy]"
    ]

spec :: Spec
spec = do
    describe "day4" $ do
        it "finds three valid rooms with sum 1514 in the sample data" $ do
            day4 (unlines sampleInputs) `shouldBe` 1514
        it "calculates sum of 158835 for actual input" $ do
            actualInput <- readFile "inputs/day4.txt"
            day4 actualInput `shouldBe` 158835
    describe "day4'" $ do
        it "finds 'northpole object storage' in sector id 993" $ do
            actualInput <- readFile "inputs/day4.txt"
            day4' actualInput `shouldBe` 993
    describe "decrypt" $ do
        it "works for sample input" $ do
            decrypt (RoomData ["qzmt", "zixmtkozy", "ivhz"] 343 "") `shouldBe` "very encrypted name"

