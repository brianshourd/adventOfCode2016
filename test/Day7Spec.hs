module Day7Spec (spec) where

import Day7
import Test.Hspec

main :: IO ()
main = hspec spec

sampleInput :: [String]
sampleInput =
    [ "abba[mnop]qrst"
    , "abcd[bddb]xyyx"
    , "aaaa[qwer]tyui"
    , "ioxxoj[asdfgh]zxcvbn"
    ]

sampleInput' :: [String]
sampleInput' =
    [ "aba[bab]xyz"
    , "xyx[xyx]xyx"
    , "aaa[kek]eke"
    , "zazbz[bzb]cdb"
    ]

spec :: Spec
spec = do
    describe "day7" $ do
        it "finds 2 ips that support TLS in the sample input" $ do
            day7 (unlines sampleInput) `shouldBe` 2
        it "finds 105 ips that support TLS in the actual input" $ do
            actualInput <- readFile "inputs/day7.txt"
            day7 actualInput `shouldBe` 105
    describe "day7'" $ do
        it "finds 3 ips that support SSL in the sample input" $ do
            day7' (unlines sampleInput') `shouldBe` 3
        it "finds 258 ips that support SSL in the actual input" $ do
            actualInput <- readFile "inputs/day7.txt"
            day7' actualInput `shouldBe` 258
