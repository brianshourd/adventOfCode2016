{-# LANGUAGE OverloadedStrings #-}
module Day1 (day1, day1', run) where

import Data.List (foldl')
import Data.Set (Set)
import qualified Data.Set as Set (empty, member, insert)
import Text.Parsec
    ( ParseError
    , char
    , choice
    , digit
    , many1
    , parse
    , sepBy1
    , string
    )

-- Data types
data TurnDirection = TurnRight | TurnLeft deriving (Eq, Ord, Show)
data Instruction = Instruction TurnDirection Int deriving (Eq, Ord, Show)
type Direction = (Int, Int)
type Location = (Int, Int)

parseInput :: String -> Either ParseError [Instruction]
parseInput = parse (sepBy1 parseInstruction (string ", ")) ""
  where
    parseInstruction = Instruction <$> parseTurnDirection <*> (read <$> many1 digit)
    parseTurnDirection = choice [TurnLeft <$ char 'L', TurnRight <$ char 'R']

distance :: Location -> Int
distance (a, b) = (abs a) + (abs b)

turn :: Direction -> TurnDirection -> Direction
turn (a, b) TurnRight = (b, -a)
turn (a, b) TurnLeft  = (-b, a)

goForward :: Direction -> Location -> Int -> [Location]
goForward (a, b) (x, y) s = map (\t -> (x + (s - t) * a, y + (s - t) * b)) [0..(s-1)]

-- Returns all of the locations visited, most recent first
-- This is the core of the problem
follow :: [Instruction] -> [Location]
follow = snd . foldl' go ((0, 1), [(0, 0)])
  where
    go :: (Direction, [Location]) -> Instruction -> (Direction, [Location])
    go (d, ls) (Instruction t s) = let d' = turn d t in (d', (goForward d' (head ls) s) ++ ls)

firstVisitedTwice :: [Instruction] -> Maybe Location
firstVisitedTwice = either Just (const Nothing) . foldr haveSeen (Right Set.empty) . follow
  where
    haveSeen :: Location -> Either Location (Set Location) -> Either Location (Set Location)
    haveSeen _ (Left l) = Left l
    haveSeen l (Right s) = if Set.member l s then Left l else Right $ Set.insert l s

-- Final, top-level exports
day1 :: String -> Int
day1 = either (const (-1)) (distance . head . follow) . parseInput

day1' :: String -> Int
day1' = either (const (-1)) (maybe (-2) id . fmap distance . firstVisitedTwice) . parseInput

-- Read from input file
run :: IO ()
run = do
    putStrLn "Day 1 results: "
    input <- readFile "inputs/day1.txt"
    putStrLn $ "  " ++ show (day1 input)
    putStrLn $ "  " ++ show (day1' input)
