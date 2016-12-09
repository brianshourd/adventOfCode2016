{-# LANGUAGE OverloadedStrings #-}
module Day9 (day9, day9', run, Expr(..), parseInput) where

import Data.Function (fix)
import Text.Parsec ((<|>) , Parsec , ParseError)
import qualified Text.Parsec as P

data Expr =
    CompressExpr
        Int -- Characters to repeat
        Int -- Number of times to repeat
        Int -- Length of marker (number of characters)
    | LiteralExpr Int -- Length of data
    deriving (Eq, Ord, Show)
type Compressed = [Expr]

parseInput :: String -> Either ParseError Compressed
parseInput = P.parse (P.many1 (P.try parseCompressExpr <|> parseLiteralExpr)) ""

parseCompressExpr :: Parsec String () Expr
parseCompressExpr = do
    start <- P.sourceColumn <$> P.getPosition
    take <- read <$> (P.char '(' *> P.many1 P.digit <* P.char 'x')
    repeat <- read <$> (P.many1 P.digit <* P.char ')')
    end <- P.sourceColumn <$> P.getPosition
    return $ CompressExpr take repeat (end - start)

parseLiteralExpr :: Parsec String () Expr
parseLiteralExpr = LiteralExpr . length <$> P.many1 P.upper

lengthDecompressedV1 :: Compressed -> Int
lengthDecompressedV1 = lengthDecompressed sumExprs
  where
    sumExprs = sum . map exprLength
    exprLength (CompressExpr _ _ l) = l
    exprLength (LiteralExpr l) = l

lengthDecompressedV2 :: Compressed -> Int
lengthDecompressedV2 = fix lengthDecompressed

-- First argument is function to use for recursing to process duplicated data
lengthDecompressed :: (Compressed -> Int) -> Compressed -> Int
lengthDecompressed recur ((CompressExpr t n _) : ds) =
    let (repeated, rest) = takeCharacters t ds in
    (n * (recur repeated)) + (lengthDecompressed recur rest)
lengthDecompressed recur ((LiteralExpr n) : ds) = n + (lengthDecompressed recur ds)
lengthDecompressed _ [] = 0

takeCharacters :: Int -> Compressed -> (Compressed, Compressed)
takeCharacters 0 ds = ([], ds)
takeCharacters n (r@(LiteralExpr k) : ds)
    | n < k = ([LiteralExpr n], (LiteralExpr (k - n)) : ds)
    | n >= k = let (a, b) = takeCharacters (n - k) ds in (r : a, b)
takeCharacters n (c@(CompressExpr _ _ l) : ds)
    | n < l = undefined -- maybe should be ([LiteralExpr n], (LiteralExpr (n - l)) : ds)
    | n >= l = let (a, b) = takeCharacters (n - l) ds in (c : a, b)

-- Final, top-level exports
day9 :: String -> Int
day9 = either (const (-1)) lengthDecompressedV1 . parseInput

day9' :: String -> Int
day9' = either (const (-1)) lengthDecompressedV2. parseInput

-- Input
run :: IO ()
run = do
    putStrLn "Day 9 results: "
    input <- readFile "inputs/day9.txt"
    putStrLn $ "  " ++ show (day9 input)
    putStrLn $ "  " ++ show (day9' input)
