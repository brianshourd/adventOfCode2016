{-# LANGUAGE OverloadedStrings #-}
module Day10 (day10, day10', run, Chip(..), Destination(..), Expr(..), parseInput, detectBot) where

import Data.Either (rights)
import Data.List (find, nub, sort)
import Data.Map ((!), Map)
import qualified Data.Map as Map
import Text.Parsec ((<|>) , Parsec , ParseError)
import qualified Text.Parsec as P

newtype Chip = Chip Int deriving (Eq, Show)
instance Ord Chip where compare (Chip a) (Chip b) = compare a b
data Destination = RobotDest Int | OutputDest Int deriving (Eq, Ord, Show)
data Expr =
    SourceExpr Chip Destination
    | BotExpr Int Destination Destination
    deriving (Eq, Ord, Show)

parseInput :: String -> [Expr]
parseInput = rights . map (P.parse parseExpr "") . lines
  where
    parseExpr = parseSource <|> parseBot
    parseSource = do
        P.try (P.string "value ")
        v <- parseInt
        P.string " goes to "
        d <- parseDestination
        return $ SourceExpr (Chip v) d
    parseDestination = parseRobot <|> parseOutput
    parseRobot = RobotDest <$> (P.try (P.string "bot ") *> parseInt)
    parseOutput = OutputDest <$> (P.try (P.string "output ") *> parseInt)
    parseBot = do
        P.try (P.string "bot ")
        n <- parseInt
        P.string " gives low to "
        d1 <- parseDestination
        P.string " and high to "
        d2 <- parseDestination
        return $ BotExpr n d1 d2
    parseInt = read <$> P.many1 P.digit

calculateComparisons :: [Expr] -> Map Int (Chip, Chip)
calculateComparisons es = comparisonMap
  where
    comparisonMap = Map.fromList . map (\i -> (i, calculateComparisonInner i)) $ allBots es
    calculateComparisonMemo bot = comparisonMap ! bot
    calculateComparisonInner bot = let (e1, e2) = incomingEdges bot in (calc e1, calc e2)
      where
        calc :: Expr -> Chip
        calc (BotExpr n d1 _) = calculateEdgeWeightInner n (isRobotDestination d1 bot)
        calc (SourceExpr c _) = c
    calculateEdgeWeightInner source which = let (a, b) = calculateComparisonMemo source in (provide which) a b
      where
        provide False = max
        provide True = min
    incomingEdges bot = let (a : b : []) = allIncomingEdges in (a, b)
      where
        allIncomingEdges = filter isIncomingEdge es
        isIncomingEdge (SourceExpr _ d) = isRobotDestination d bot
        isIncomingEdge (BotExpr _ d1 d2) = isRobotDestination d1 bot || isRobotDestination d2 bot
    isRobotDestination (RobotDest n) bot = n == bot
    isRobotDestination _ _ = False

detectBot :: [Expr] -> (Chip, Chip) -> Int
detectBot es (c1, c2) = fst . head . filter (isMatch . snd) . Map.toList . calculateComparisons $ es
  where
    isMatch (a, b) = (a == c1 && b == c2) || (a == c2 && b == c1)

allBots :: [Expr] -> [Int]
allBots = sort . nub . concatMap getBots
  where
    getBots (SourceExpr _ d) = getBotsFromDest d
    getBots (BotExpr n1 d1 d2) = n1 : (getBotsFromDest d1 ++ getBotsFromDest d2)
    getBotsFromDest (RobotDest n) = [n]
    getBotsFromDest _ = []

allOutputs :: [Expr] -> [Int]
allOutputs = sort . nub . concatMap getOutputs
  where
    getOutputs (SourceExpr _ d) = getOutputsFromDes d
    getOutputs (BotExpr n1 d1 d2) = n1 : (getOutputsFromDes d1 ++ getOutputsFromDes d2)
    getOutputsFromDes (OutputDest n) = [n]
    getOutputsFromDes _ = []

calculateOutputs :: [Expr] -> Map Int Chip
calculateOutputs es = Map.fromList . map (\i -> (i, calculateOutput i)) $ allOutputs es
  where
    bots = calculateComparisons es
    calculateOutput output = case incomingEdge output of
        (SourceExpr c _) -> c
        (BotExpr n d1 d2) -> let (c1, c2) = bots ! n in
            if (d1 == OutputDest output) then min c1 c2 else max c1 c2
    incomingEdge output = head . filter isIncomingEdge $ es
      where
        isIncomingEdge (SourceExpr _ d) = isOutputDestination d output
        isIncomingEdge (BotExpr _ d1 d2) = isOutputDestination d1 output || isOutputDestination d2 output
    isOutputDestination (OutputDest n) output = n == output
    isOutputDestination _ _ = False

-- Final, top-level exports
day10 :: String -> Int
day10 = flip detectBot (Chip 61, Chip 17) . parseInput

day10' :: String -> Int
day10' input = product $ map (\i -> extractValue $ outputs ! i) [0..2]
  where
    outputs = calculateOutputs $ parseInput input
    extractValue (Chip n) = n

-- Input
run :: IO ()
run = do
    putStrLn "Day 10 results: "
    input <- readFile "inputs/day10.txt"
    putStrLn $ "  " ++ show (day10 input)
    putStrLn $ "  " ++ show (day10' input)
