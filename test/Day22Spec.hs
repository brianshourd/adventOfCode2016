module Day22Spec (spec) where

import Day22

import Data.Array (Array)
import qualified Data.Array as Array
import Test.Hspec

main :: IO ()
main = hspec spec

sampleInput :: String
sampleInput = unlines
    [ "root@ebhq-gridcenter# df -h"
    , "Filesystem            Size  Used  Avail  Use%"
    , "/dev/grid/node-x0-y0   10T    8T     2T   80%"
    , "/dev/grid/node-x0-y1   11T    6T     5T   54%"
    , "/dev/grid/node-x0-y2   32T   28T     4T   87%"
    , "/dev/grid/node-x1-y0    9T    7T     2T   77%"
    , "/dev/grid/node-x1-y1    8T    0T     8T    0%"
    , "/dev/grid/node-x1-y2   11T    7T     4T   63%"
    , "/dev/grid/node-x2-y0   10T    6T     4T   60%"
    , "/dev/grid/node-x2-y1    9T    8T     1T   88%"
    , "/dev/grid/node-x2-y2    9T    6T     3T   66%"
    ]

sampleParsed :: Grid
sampleParsed = Array.array ((0, 0), (2, 2))
    [ ((0, 0), Node 10  8 2)
    , ((0, 1), Node 11  6 5)
    , ((0, 2), Node 32 28 4)
    , ((1, 0), Node  9  7 2)
    , ((1, 1), Node  8  0 8)
    , ((1, 2), Node 11  7 4)
    , ((2, 0), Node 10  6 4)
    , ((2, 1), Node  9  8 1)
    , ((2, 2), Node  9  6 3)
    ]

spec :: Spec
spec = do
    describe "parseInput" $ do
        it "works for sample input" $ do
            parseInput sampleInput `shouldBe` sampleParsed
    describe "day22" $ do
        it "works for sample input" $ do
            day22 sampleInput `shouldBe` 7
        it "works for actual input" $ do
            actualInput <- readFile "inputs/day22.txt"
            day22 actualInput `shouldBe` 903
    describe "day22'" $ do
        it "works for actual input" $ do
--            actualInput <- readFile "inputs/day22.txt"
--            day22' actualInput `shouldBe` "fdhgacbe"
            pending
