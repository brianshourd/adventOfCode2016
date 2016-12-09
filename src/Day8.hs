{-# LANGUAGE OverloadedStrings #-}
module Day8 (day8, day8', run, Instruction(..), Screen, applyInstruction, parseInput) where

import Data.Array.Unboxed ((//), (!), UArray, bounds, elems, listArray, range)
import Text.Parsec ((<|>) , Parsec , ParseError)
import qualified Text.Parsec as P

data Instruction = Rect Int Int | RotateRow Int Int | RotateColumn Int Int deriving (Eq, Ord, Show)
type Screen = UArray (Int, Int) Bool

parseInput :: String -> Either ParseError [Instruction]
parseInput = P.parse (P.sepEndBy1 parseInstruction P.endOfLine) ""

parseInstruction :: Parsec String () Instruction
parseInstruction = P.try parseRect <|> P.try parseRotateRow <|> parseRotateColumn
  where
    parseRect = Rect <$> (P.string "rect " *> number) <*> (P.char 'x' *> number)
    parseRotateRow = RotateRow <$> (P.string "rotate row y=" *> number) <*> (P.string " by " *> number)
    parseRotateColumn = RotateColumn <$> (P.string "rotate column x=" *> number) <*> (P.string " by " *> number)
    number = read <$> P.many1 P.digit

applyInstruction :: Screen -> Instruction -> Screen
applyInstruction s (Rect x y) = s // [(i, True) | i <- range ((0, 0), (x - 1, y - 1))]
applyInstruction s (RotateRow y n) = s // [(i, s ! (shift i)) | i <- range ((0, y), (maxX, y))]
  where
    (_, (maxX, _)) = bounds s
    shift (x, y) = ((x - n) `mod` (maxX + 1), y)
applyInstruction s (RotateColumn x n) = s // [(i, s ! (shift i)) | i <- range ((x, 0), (x, maxY))]
  where
    (_, (_, maxY)) = bounds s
    shift (x, y) = (x, (y - n) `mod` (maxY + 1))

empty :: Screen
empty = listArray ((0, 0), (49, 5)) (repeat False)

showScreen :: Screen -> [String]
showScreen s = [ [if s ! (x, y) then '#' else '.' | x <- [0 .. maxX]] | y <- [0 .. maxY] ]
  where
    (_, (maxX, maxY)) = bounds s

-- Final, top-level exports
day8 :: String -> Int
day8 = either (const (-1)) (length . filter id . elems . foldl applyInstruction empty) . parseInput

day8' :: String -> Screen
day8' = either (const empty) (foldl applyInstruction empty) . parseInput

-- Input
run :: IO ()
run = do
    putStrLn "Day 8 results: "
    input <- readFile "inputs/day8.txt"
    putStrLn $ "  " ++ show (day8 input)
    mapM_ (putStrLn . ("  " ++)) . showScreen . day8' $ input
