{-# LANGUAGE OverloadedStrings #-}
module Day1 (day1, day1', run, firstVisitedTwice, Instruction(..), takeSteps) where

import Data.Attoparsec.Text
    ( Parser(..)
    , char
    , choice
    , decimal
    , eitherP
    , parseOnly
    , sepBy1
    , string
    )
import Data.List (foldl')
import Data.Set (Set)
import qualified Data.Set as Set (empty, member, insert)
import Data.Text (pack)

-- Data types
data Instruction = R Int | L Int deriving (Eq, Ord, Show)
type Direction = (Int, Int)
type Location = (Int, Int)

-- Parse input
parseInput :: String -> Either String [Instruction]
parseInput input = parseOnly parseInstructions (pack input)
  where
    parseInstructions = sepBy1 parseInstruction (string ", ")
    parseInstruction = do
        constructor <- choice [(char 'L') >> pure L, (char 'R') >> pure R]
        steps <- decimal
        return $ constructor steps

rotate :: Direction -> Instruction -> Direction
rotate (a, b) (R _) = (b, -a)
rotate (a, b) (L _) = (-b, a)

steps :: Instruction -> Int
steps (R s) = s
steps (L s) = s

-- Returns all of the locations visited, most recent first
takeSteps :: Direction -> Location -> Int -> [Location]
takeSteps (a, b) (x, y) s = take s . iterate (\(x', y') -> (x' - a, y' - b)) $ (x + s * a, y + s * b)

distance :: Location -> Int
distance (a, b) = (abs a) + (abs b)

-- Returns all of the locations visited, most recent first
allLocations :: [Instruction] -> [Location]
allLocations = snd . foldl' follow1 ((0, 1), [(0, 0)])
  where
    follow1 :: (Direction, [Location]) -> Instruction -> (Direction, [Location])
    follow1 (d, ls) i = let d' = rotate d i in (d', (takeSteps d' (head ls) (steps i)) ++ ls)

firstVisitedTwice :: [Instruction] -> Maybe Location
firstVisitedTwice = either Just (const Nothing) . foldr haveSeen (Right Set.empty) . allLocations
  where
    haveSeen :: Location -> Either Location (Set Location) -> Either Location (Set Location)
    haveSeen l (Right s) = if Set.member l s then Left l else Right $ Set.insert l s
    haveSeen _ (Left l) = Left l

-- Final, top-level exports
day1 :: String -> Int
day1 = either (const (-1)) (distance . head . allLocations) . parseInput

day1' :: String -> Int
day1' = either (const (-1)) (maybe (-2) id . fmap distance . firstVisitedTwice) . parseInput

-- Read from input file
run :: IO ()
run = do
    putStrLn "Day 1 results: "
    input <- readFile "inputs/day1.txt"
    putStrLn $ "  " ++ show (day1 input)
    putStrLn $ "  " ++ show (day1' input)
