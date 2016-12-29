module AssembunnySpec (spec) where

import Assembunny
import Test.Hspec

main :: IO ()
main = hspec spec

sampleInput1 :: String
sampleInput1 = unlines
    [ "cpy 2 a"
    , "tgl a"
    , "tgl a"
    , "tgl a"
    , "cpy 1 a"
    , "dec a"
    , "dec a"
    ]

sampleParsed1 :: [Expr]
sampleParsed1 =
    [ CopyExpr (Literal 2) (Reference (Register 'a'))
    , ToggleExpr (Register 'a')
    , ToggleExpr (Register 'a')
    , ToggleExpr (Register 'a')
    , CopyExpr (Literal 1) (Reference (Register 'a'))
    , DecExpr (Register 'a')
    , DecExpr (Register 'a')
    ]

sampleInput2 :: String
sampleInput2 = unlines
    [ "cpy 41 a"
    , "inc a"
    , "inc a"
    , "dec a"
    , "jnz a 2"
    , "dec a"
    ]

sampleParsed2 :: [Expr]
sampleParsed2 =
    [ CopyExpr (Literal 41) (Reference (Register 'a'))
    , IncExpr (Register 'a')
    , IncExpr (Register 'a')
    , DecExpr (Register 'a')
    , JumpExpr (Reference (Register 'a')) (Literal 2)
    , DecExpr (Register 'a')
    ]

spec :: Spec
spec = do
    describe "parseInput" $ do
        it "parses sample input 1 properly" $ do
            parseInput sampleInput1 `shouldBe` sampleParsed1
        it "parses sample input 2 properly" $ do
            parseInput sampleInput2 `shouldBe` sampleParsed2
    describe "evaluate" $ do
        it "processes sample input 1 to find 3 in register a, when a starts at 7" $ do
            let initial = writeRegister 'a' 7 $ start sampleParsed1
            readRegister 'a' (evaluate initial) `shouldBe` 3
        it "processes sample input 2 to find 42 in register a" $ do
            let initial = start sampleParsed2
            readRegister 'a' (evaluate initial) `shouldBe` 42
--        it "works on actual input" $ do
--            actualInput <- readFile "inputs/day23.txt"
--            day23 actualInput `shouldBe` 317993
