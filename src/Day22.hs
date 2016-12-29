module Day22 where

import Data.Array ((!), Array)
import qualified Data.Array as Array
import Data.Either (rights)
import Text.Parsec ((<|>) , Parsec , ParseError)
import qualified Text.Parsec as P

data Node = Node
    { nodeSize :: Int
    , nodeUsed :: Int
    , nodeAvail :: Int
    } deriving (Eq, Ord, Show)
type Position = (Int, Int)
type Grid = Array Position Node

parseInput :: String -> Grid
parseInput input = let lines = parseLines input in Array.array (bounds lines) lines
  where
    parseLines = rights . map (P.parse parseLine "") . drop 2 . lines
    parseLine = do
        pos <- (,) <$> (P.string "/dev/grid/node-x" *> number) <*> (P.string "-y" *> number <* P.spaces)
        node <- Node <$> unit <*> unit <*> unit
        return (pos, node)
      where
        unit = number <* P.char 'T' <* P.spaces
    bounds xs = let is = map fst xs in (minimum is, maximum is)

number :: Parsec String () Int
number = read <$> P.many1 P.digit

viablePairs :: Grid -> [(Node, Node)]
viablePairs g = let nodes = Array.elems g in
    [(a, b) | a <- nodes, b <- nodes, a /= b, (nodeUsed a) > 0, (nodeUsed a) <= (nodeAvail b)]

printGrid :: Grid -> [String]
printGrid g =
    [
        [showNode ((x, y), g ! (x, y)) | x <- [0..37]]
        | y <- [0..23]
    ]
  where
    showNode ((x, y), (Node s u a))
        | x == 0 && y == 0 = 'O'
        | x == 37 && y == 0 = 'G'
        | u == 0 = '_'
        | u > 80 = '#'
        | s > 80 = '.'
        | otherwise = '?'

-- Final, top-level exports
day22 :: String -> Int
day22 = length . viablePairs . parseInput

-- Input
run :: IO ()
run = do
    putStrLn "Day 22 results: "
    input <- readFile "inputs/day22.txt"
    putStrLn $ "  " ++ show (day22 input)
    putStrLn $ "  215 (manual - just look at the picture)"
    putStrLn . unlines . map ("  " ++) . printGrid . parseInput $ input
