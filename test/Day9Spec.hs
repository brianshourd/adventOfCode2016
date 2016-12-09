module Day9Spec (spec) where

import Day9
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "day9" $ do
        it "decompresses 'ADVENT' to length 6" $ do
            day9 "ADVENT" `shouldBe` 6
        it "decompresses 'A(1x5)BC' to length 7" $ do
            day9 "A(1x5)BC" `shouldBe` 7
        it "decompresses '(3x3)XYZ' to length 9" $ do
            day9 "(3x3)XYZ" `shouldBe` 9
        it "decompresses 'A(2x2)BCD(2x2)EFG' to length 11" $ do
            day9 "A(2x2)BCD(2x2)EFG" `shouldBe` 11
        it "decompresses '(6x1)(1x3)A' to length 6" $ do
            day9 "(6x1)(1x3)A" `shouldBe` 6
        it "decompresses 'X(8x2)(3x3)ABCY' to length 18" $ do
            day9 "X(8x2)(3x3)ABCY" `shouldBe` 18
        it "decompresses the actual input to length 138735" $ do
            actualInput <- readFile "inputs/day9.txt"
            day9 actualInput `shouldBe` 138735
    describe "parseInput" $ do
        it "parses '(3x3)XYZ' correctly" $ do
            parseInput "(3x3)XYZ" `shouldBe` Right
                [ CompressExpr 3 3 5
                , LiteralExpr 3
                ]
        it "parses 'X(8x2)(3x3)ABCY' correctly" $ do
            parseInput "X(8x2)(3x3)ABCY" `shouldBe` Right
                [ LiteralExpr 1
                , CompressExpr 8 2 5
                , CompressExpr 3 3 5
                , LiteralExpr 4
                ]
        it "parses '(27x12)(20x12)(13x14)(7x10)(1x12)A' correctly" $ do
            parseInput "(27x12)(20x12)(13x14)(7x10)(1x12)A" `shouldBe` Right
                [ CompressExpr 27 12 7
                , CompressExpr 20 12 7
                , CompressExpr 13 14 7
                , CompressExpr 7 10 6
                , CompressExpr 1 12 6
                , LiteralExpr 1
                ]
        it "parses '(25x3)(3x3)ABC(2x3)XY(5x2)PQRSTX(18x9)(3x2)TWO(5x7)SEVEN' correctly" $ do
            parseInput "(25x3)(3x3)ABC(2x3)XY(5x2)PQRSTX(18x9)(3x2)TWO(5x7)SEVEN" `shouldBe` Right
                [ CompressExpr 25 3 6
                , CompressExpr 3 3 5
                , LiteralExpr 3
                , CompressExpr 2 3 5
                , LiteralExpr 2
                , CompressExpr 5 2 5
                , LiteralExpr 6
                , CompressExpr 18 9 6
                , CompressExpr 3 2 5
                , LiteralExpr 3
                , CompressExpr 5 7 5
                , LiteralExpr 5
                ]
    describe "day9'" $ do
        it "counts 9 characters in '(3x3)XYZ'" $ do
            day9' "(3x3)XYZ" `shouldBe` 9
        it "counts 20 characters in 'X(8x2)(3x3)ABCY'" $ do
            day9' "X(8x2)(3x3)ABCY" `shouldBe` 20
        it "counts 241920 characters in '(27x12)(20x12)(13x14)(7x10)(1x12)A'" $ do
            day9' "(27x12)(20x12)(13x14)(7x10)(1x12)A" `shouldBe` 241920
        it "counts 445 characters in '(25x3)(3x3)ABC(2x3)XY(5x2)PQRSTX(18x9)(3x2)TWO(5x7)SEVEN'" $ do
            day9' "(25x3)(3x3)ABC(2x3)XY(5x2)PQRSTX(18x9)(3x2)TWO(5x7)SEVEN" `shouldBe` 445
        it "counts 11125026826 characters in the actual input" $ do
            actualInput <- readFile "inputs/day9.txt"
            day9' actualInput `shouldBe` 11125026826
