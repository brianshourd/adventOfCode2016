{-# LANGUAGE OverloadedStrings #-}
module Day2 (day2, day2', run) where

import Text.Parsec
    ( ParseError
    , char
    , choice
    , endOfLine
    , many1
    , optional
    , parse
    )

-- Imagine a 2D grid, with 5 at (0, 0)
type Direction = (Int, Int)
type Location = (Int, Int)
type BoundsFunction = Location -> Bool

-- Parse letters into directions
parseInput :: String -> Either ParseError [[Direction]]
parseInput = parse (many1 parseLine) ""
  where
    parseLine = many1 (choice
        [ (0, 1)  <$ char 'U'
        , (0, -1) <$ char 'D'
        , (1, 0)  <$ char 'R'
        , (-1, 0) <$ char 'L'
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
boundsFunction (x, y) = (abs x) <= 1 && (abs y) <= 1

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
day2 = either show id . fmap ((map lookupValue) . (calculateCode boundsFunction)) . parseInput

day2' :: String -> String
day2' = either show id . fmap ((map lookupValue') . (calculateCode boundsFunction')) . parseInput

-- Read from input file
run :: IO ()
run = do
    putStrLn "Day 2 results: "
    input <- readFile "inputs/day2.txt"
    putStrLn $ "  " ++ show (day2 input)
    putStrLn $ "  " ++ show (day2' input)
