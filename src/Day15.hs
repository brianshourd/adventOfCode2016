{-# LANGUAGE OverloadedStrings #-}
module Day15 where

import Data.Either (rights)
import Data.List (sortOn)
import Text.Parsec ((<|>) , Parsec , ParseError)
import qualified Text.Parsec as P

data Disc = Disc
    { discIndex :: Int
    , discPositions :: Int
    , discOffset :: Int
    } deriving (Eq, Show)

parseInput :: String -> [Disc]
parseInput = rights . map (P.parse parseDisc "") . lines

parseDisc :: Parsec String () Disc
parseDisc = Disc <$>
    (P.string "Disc #" *> number) <*>
    (P.string " has " *> number) <*>
    (P.string " positions; at time=0, it is at position " *> number <* P.char '.')
  where
    number = read <$> P.many1 P.digit

makeRequirement :: Disc -> Requirement
makeRequirement (Disc i p o) = Requirement p ((-(i + o)) `mod` p)

data Requirement = Requirement
    { reqBase :: Int
    , reqRemainder :: Int
    } deriving (Eq, Ord, Show)

doesSatisfy :: Requirement -> Int -> Bool
doesSatisfy (Requirement p r) i = (i `mod` p) == r

combine :: Requirement -> Requirement -> Requirement
combine r@(Requirement p _) r'@(Requirement p' _) = Requirement (p * p') (satisfy [r, r'])

satisfy :: [Requirement] -> Int
satisfy [] = 0
satisfy (r : []) = reqRemainder r
satisfy ((Requirement p r) : r' : []) = head . filter (doesSatisfy r') $ map (\i -> (i * p) + r) [0..]
satisfy (r : rs) = reqRemainder $ foldl combine r rs

addDisc :: Int -> Int -> [Disc] -> [Disc]
addDisc position offset ds = (Disc (maxIndex + 1) position offset) : ds
  where
    maxIndex = maximum $ map discIndex ds

-- Final, top-level exports
day15 :: String -> Int
day15 = satisfy . reverse . sortOn reqBase . map makeRequirement . parseInput

day15' :: String -> Int
day15' = satisfy . reverse . sortOn reqBase . map makeRequirement . addDisc 11 0 . parseInput

-- Input
run :: IO ()
run = do
    putStrLn "Day 15 results: "
    input <- readFile "inputs/day15.txt"
    putStrLn $ "  " ++ show (day15 input)
    putStrLn $ "  " ++ show (day15' input)
