module Day20 where

import Data.Either (rights)
import Data.List (sortOn)
import Text.Parsec ((<|>) , Parsec , ParseError)
import qualified Text.Parsec as P

type Range = (Int, Int)

parseInput :: String -> [Range]
parseInput = rights . map (P.parse parseRange "") . lines
  where
    parseRange = (,) <$> (number <* P.char '-') <*> number
    number = read <$> P.many1 P.digit

firstValid :: [Range] -> Int
firstValid = go (-1) . sortOn fst
  where
    go :: Int -> [Range] -> Int
    go m ((x, y) : xs) = if (x > m + 1) then m + 1 else go (max m y) xs
    go m []            = m + 1

countValid :: Int -> [Range] -> Int
countValid highest rs = go (-1) (sortOn fst ((highest + 1, highest + 2) : rs)) 0
  where
    go :: Int -> [Range] -> Int -> Int
    go m ((x, y) : xs) c = if (x > m + 1) then go y xs (c + x - m - 1) else go (max m y) xs c
    go _ []            c = c

-- Final, top-level exports
day20 :: String -> Int
day20 = firstValid . parseInput

day20' :: String -> Int
day20' = countValid 4294967295 . parseInput

-- Input
run :: IO ()
run = do
    putStrLn "Day 20 results: "
    input <- readFile "inputs/day20.txt"
    putStrLn $ "  " ++ show (day20 input)
    putStrLn $ "  " ++ show (day20' input)
