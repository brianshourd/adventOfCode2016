{-# LANGUAGE OverloadedStrings #-}
module Day12 (day12, day12', run, Expr(..), Value(..), Register(..), parseInput) where

import Control.Monad.ST (ST, runST)
import Data.Either (rights)
import Data.Map ((!), Map)
import qualified Data.Map as Map
import Data.Vector ((!?), Vector)
import qualified Data.Vector as Vector
import Text.Parsec ((<|>) , Parsec , ParseError)
import qualified Text.Parsec as P

data Value = Literal Int | Reference Register deriving (Eq, Ord, Show)
newtype Register = Register Char deriving (Eq, Ord, Show)
data Expr =
    CopyExpr Value Register
    | IncExpr Register
    | DecExpr Register
    | JumpExpr Value Int
    deriving (Eq, Ord, Show)

parseInput :: String -> [Expr]
parseInput = rights . map (P.parse parseExpr "") . lines

parseExpr :: Parsec String () Expr
parseExpr = parseCopyExpr <|> parseIncExpr <|> parseDecExpr <|> parseJumpExpr
  where
    parseCopyExpr = CopyExpr <$> ((P.try $ P.string "cpy ") *> parseValue <* P.spaces) <*> parseRegister
    parseIncExpr = IncExpr <$> ((P.try $ P.string "inc ") *> parseRegister)
    parseDecExpr = DecExpr <$> ((P.try $ P.string "dec ") *> parseRegister)
    parseJumpExpr = JumpExpr <$> ((P.try $ P.string "jnz ") *> parseValue <* P.spaces) <*> parseInt
    parseRegister = Register <$> P.oneOf "abcd"
    parseValue = (P.try (Literal <$> parseInt)) <|> (Reference <$> parseRegister)

parseInt :: Parsec String () Int
parseInt = do
    minus <- P.many (P.char '-')
    digits <- P.many1 P.digit
    return $ read (minus ++ digits)

data Execution = Execution
    { lineNum :: Int
    , registers :: Map Register Int
    } deriving (Eq, Ord, Show)

start :: Execution
start = Execution 0 $ Map.fromList [(r, 0) | r <- map Register "abcd"]

start' :: Execution
start' = start { registers = Map.insert (Register 'c') 1 (registers start) }

evaluate :: Execution -> Vector Expr -> Execution
evaluate e es = case es !? (lineNum e) of
    Just expr -> evaluate (evaluate1 expr e) es
    Nothing -> e
  where
    upperBound = length es
    inBounds e = let line = (lineNum e) in line < upperBound && line >= 0

evaluate1 :: Expr -> Execution -> Execution
evaluate1 (CopyExpr v r) e = incLineNum $ setValue e r (getValue e v)
evaluate1 (IncExpr r) e = incLineNum $ setValue e r ((getValue e (Reference r)) + 1)
evaluate1 (DecExpr r) e = incLineNum $ setValue e r ((getValue e (Reference r)) - 1)
evaluate1 (JumpExpr v n) e = if getValue e v > 0 then e { lineNum = (lineNum e + n) } else incLineNum e

incLineNum :: Execution -> Execution
incLineNum e = e { lineNum = (lineNum e) + 1 }

getValue :: Execution -> Value -> Int
getValue e (Literal n) = n
getValue e (Reference r) = (registers e) ! r

setValue :: Execution -> Register -> Int -> Execution
setValue e r n = e { registers = Map.insert r n (registers e) }

-- Final, top-level exports
day12 :: String -> Int
day12 = (! (Register 'a')) . registers . evaluate start . Vector.fromList . parseInput

day12' :: String -> Int
day12' = (! (Register 'a')) . registers . evaluate start' . Vector.fromList . parseInput

-- Input
run :: IO ()
run = do
    --putStrLn "Day 12 results: "
    --input <- readFile "inputs/day12.txt"
    --putStrLn $ "  " ++ show (day12 input)
    --putStrLn $ "  " ++ show (day12' input)
    putStrLn "Day 12 results (cached): "
    putStrLn $ "  317993"
    putStrLn $ "  9227647"
