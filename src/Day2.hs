{-# LANGUAGE OverloadedStrings #-}
module Day2 (day2, day2', run) where

import Control.Applicative (optional)
import Data.Attoparsec.Text (Parser(..), char, choice, endOfLine, many1, parseOnly)
import Data.Text (pack)

-- Imagine a 2D grid, with 5 at (0, 0)
type Direction = (Int, Int)
type Location = (Int, Int)
type BoundsFunction = Location -> Bool

-- Parse letters into directions
parseInput :: String -> Either String [[Direction]]
parseInput input = parseOnly (many1 parseLine) (pack input)
  where
    parseLine :: Parser [Direction]
    parseLine = many1 (choice
        [ char 'U' >> pure (0, 1)
        , char 'D' >> pure (0, -1)
        , char 'R' >> pure (1, 0)
        , char 'L' >> pure (-1, 0)
        ]) <* optional endOfLine

-- Core functionality
calculateCode :: BoundsFunction -> [[Direction]] -> [Location]
calculateCode inBounds = tail . reverse . foldl go [(0, 0)]
  where
    go ls@(l : _) ds = (foldl move l ds) : ls
    move l@(x, y) (a, b) = let l' = (x + a, y + b) in
        if inBounds l' then l' else l

-- Describing constraints of part 1
boundsFunction :: Location -> Bool
boundsFunction (x, y) = x <= 1 && x >= -1 && y <= 1 && y >= -1

lookupValue :: Location -> Char
lookupValue (x, y) = (table !! (y + 1)) !! (x + 1)
  where
    table = ["789", "456", "123"]

-- Describing constraints of part 2
boundsFunction' :: Location -> Bool
boundsFunction' (x, y) = y <= x && y <= (4 - x) && y >= -x && y >= (x - 4)

lookupValue' :: Location -> Char
lookupValue' (x, y) = (table !! (y + 2)) !! x
  where
    table = ["??D??", "?ABC?", "56789", "?234?", "??1??"]

-- Final, top-level exports
day2 :: String -> String
day2 = either id id . fmap ((map lookupValue) . (calculateCode boundsFunction)) . parseInput

day2' :: String -> String
day2' = either id id . fmap ((map lookupValue') . (calculateCode boundsFunction')) . parseInput

-- Read from input file
run :: IO ()
run = do
    putStrLn "Day 2 results: "
    input <- readFile "inputs/day2.txt"
    putStrLn $ "  " ++ show (day2 input)
    putStrLn $ "  " ++ show (day2' input)
