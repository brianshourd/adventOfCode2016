module Day12Spec (spec) where

import Day12
import Test.Hspec

main :: IO ()
main = hspec spec

sampleInput :: String
sampleInput = unlines
    [ "cpy 41 a"
    , "inc a"
    , "inc a"
    , "dec a"
    , "jnz a 2"
    , "dec a"
    ]

sampleParsed :: [Expr]
sampleParsed =
    [ CopyExpr (Literal 41) (Register 'a')
    , IncExpr (Register 'a')
    , IncExpr (Register 'a')
    , DecExpr (Register 'a')
    , JumpExpr (Reference $ Register 'a') 2
    , DecExpr (Register 'a')
    ]

spec :: Spec
spec = do
    describe "parseInput" $ do
        it "parses sample input properly" $ do
            parseInput sampleInput `shouldBe` sampleParsed
    describe "day12" $ do
        it "processes sample input to find 42 in register a" $ do
            day12 sampleInput `shouldBe` 42
        it "works on actual input" $ do
            actualInput <- readFile "inputs/day12.txt"
            day12 actualInput `shouldBe` 317993
    describe "day12'" $ do
        it "works on actual input" $ do
            actualInput <- readFile "inputs/day12.txt"
            --day12' actualInput `shouldBe` 9227647
            pendingWith "takes just a bit too long"
