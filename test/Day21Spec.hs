module Day21Spec (spec) where

import Day21
import Test.Hspec

main :: IO ()
main = hspec spec

sampleInput :: String
sampleInput = unlines
    [ "swap position 4 with position 0"
    , "swap letter d with letter b"
    , "reverse positions 0 through 4"
    , "rotate left 1 step"
    , "move position 1 to position 4"
    , "move position 3 to position 0"
    , "rotate based on position of letter b"
    , "rotate based on position of letter d"
    ]

spec :: Spec
spec = do
    describe "swapPositionInstruction" $ do
        it "scramble 'swap position 4 with position 0' works on 'abcde'" $ do
            scramble "abcde" (parseInput "swap position 4 with position 0") `shouldBe` "ebcda"
        it "unscramble 'swap position 4 with position 0' works on 'ebcda'" $ do
            unscramble "ebcda" (parseInput "swap position 4 with position 0") `shouldBe` "abcde"
    describe "swapLetterInstruction" $ do
        it "scramble 'swap letter d with letter b' works on 'ebcda'" $ do
            scramble "ebcda" (parseInput "swap letter d with letter b") `shouldBe` "edcba"
        it "unscramble 'swap letter d with letter b' works on 'edcba'" $ do
            unscramble "edcba" (parseInput "swap letter d with letter b") `shouldBe` "ebcda"
    describe "reverseInstruction" $ do
        it "scramble 'reverse positions 0 through 4' works on 'edcba'" $ do
            scramble "edcba" (parseInput "reverse positions 0 through 4") `shouldBe` "abcde"
        it "unscramble 'reverse positions 0 through 4' works on 'abcde'" $ do
            unscramble "abcde" (parseInput "reverse positions 0 through 4") `shouldBe` "edcba"
        it "scramble 'reverse positions 1 through 3' works on 'abcde'" $ do
            scramble "abcde" (parseInput "reverse positions 1 through 3") `shouldBe` "adcbe"
        it "unscramble 'reverse positions 1 through 3' works on 'adcbe'" $ do
            unscramble "adcbe" (parseInput "reverse positions 1 through 3") `shouldBe` "abcde"
    describe "rotateLeftInstruction" $ do
        it "scramble 'rotate left 1 step' works on 'abcde'" $ do
            scramble "abcde" (parseInput "rotate left 1 step") `shouldBe` "bcdea"
        it "unscramble 'rotate left 1 step' works on 'bcdea'" $ do
            unscramble "bcdea" (parseInput "rotate left 1 step") `shouldBe` "abcde"
    describe "rotateRightInstruction" $ do
        it "scramble 'rotate right 2 steps' works on 'abcde'" $ do
            scramble "abcde" (parseInput "rotate right 2 steps") `shouldBe` "deabc"
        it "unscramble 'rotate right 2 steps' works on 'deabc'" $ do
            unscramble "deabc" (parseInput "rotate right 2 steps") `shouldBe` "abcde"
    describe "moveInstruction" $ do
        it "scramble 'move position 1 to position 4' works on 'bcdea'" $ do
            scramble "bcdea" (parseInput "move position 1 to position 4") `shouldBe` "bdeac"
        it "unscramble 'move position 1 to position 4' works on 'bdeac'" $ do
            unscramble "bdeac" (parseInput "move position 1 to position 4") `shouldBe` "bcdea"
        it "scramble 'move position 3 to position 0' works on 'bdeac'" $ do
            scramble "bdeac" (parseInput "move position 3 to position 0") `shouldBe` "abdec"
        it "unscramble 'move position 3 to position 0' works on 'abdec'" $ do
            unscramble "abdec" (parseInput "move position 3 to position 0") `shouldBe` "bdeac"
    describe "rotatePositionInstruction" $ do
        it "scramble 'rotate based on position of letter b' works on 'abdec'" $ do
            scramble "abdec" (parseInput "rotate based on position of letter b") `shouldBe` "ecabd"
        it "scramble 'rotate based on position of letter d' works on 'ecabd'" $ do
            scramble "ecabd" (parseInput "rotate based on position of letter d") `shouldBe` "decab"
        -- note that unscramble does not work on 5-character passwords for rotatePositionInstruction
        it "unscramble 'rotate based on position of letter h' works on 'habcdefg'" $ do
            unscramble "habcdefg" (parseInput "rotate based on position of letter h") `shouldBe` "abcdefgh"
        it "unscramble 'rotate based on position of letter a' works on 'habcdefg'" $ do
            unscramble "habcdefg" (parseInput "rotate based on position of letter a") `shouldBe` "abcdefgh"
        it "unscramble 'rotate based on position of letter d' works on 'efghabcd'" $ do
            unscramble "efghabcd" (parseInput "rotate based on position of letter d") `shouldBe` "abcdefgh"
    describe "scramble" $ do
        it "works on sample input" $ do
            scramble "abcde" (parseInput sampleInput) `shouldBe` "decab"
    describe "day21" $ do
        it "works for actual input" $ do
            actualInput <- readFile "inputs/day21.txt"
            day21 actualInput `shouldBe` "dgfaehcb"
    describe "day21'" $ do
        it "works for actual input" $ do
            actualInput <- readFile "inputs/day21.txt"
            day21' actualInput `shouldBe` "fdhgacbe"
