{-# LANGUAGE OverloadedStrings #-}
module Day3 (day3, day3', run) where

import Control.Applicative (optional)
import Data.Attoparsec.Text (Parser(..), decimal, parseOnly, sepBy, skipSpace)
import Data.Either (rights)
import Data.List (sort, transpose)
import Data.List.Split (chunksOf)
import Data.Text (pack)

parseLine :: String -> Either String [Int]
parseLine = parseOnly (optional skipSpace >> decimal `sepBy` skipSpace) . pack

parseInput :: String -> [[Int]]
parseInput = rights . map parseLine . lines

isTriangle :: [Int] -> Bool
isTriangle xs = let (a:b:c:_) = sort xs in a + b > c

-- Final, top-level exports
day3 :: String -> Int
day3 = length . filter isTriangle . parseInput

day3' :: String -> Int
day3' = length . filter isTriangle . concat . map transpose . chunksOf 3 . parseInput

-- Read from input file
run :: IO ()
run = do
    putStrLn "Day 3 results: "
    input <- readFile "inputs/day3.txt"
    putStrLn $ "  " ++ show (day3 input)
    putStrLn $ "  " ++ show (day3' input)
