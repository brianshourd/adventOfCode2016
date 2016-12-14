{-# LANGUAGE OverloadedStrings #-}
module Day13 (day13, day13', run, stepsToReach, numLocationsReached) where

import Data.Bits (popCount)
import Data.List (findIndex)
import Data.Maybe (fromMaybe)
import Data.Set ((\\), Set)
import qualified Data.Set as Set

-- Note that the equation x^2 + 3x + 2xy + y + y^2 is just mapping points like
-- this:
--   0 1 2 3
--  +--------
-- 0|0 2 5 9
-- 1|1 4 8
-- 2|3 7
-- 3|6

type Seed = Int
type Location = (Int, Int)
data Zone = Zone
    { visited :: Set Location -- Locations in the interior of the zone
    , crest :: Set Location -- Locations on the boundary of the zone
    } deriving (Eq, Ord, Show)

zones :: Seed -> [Zone]
zones seed = iterate expand $ Zone Set.empty (Set.singleton (1, 1))
  where
    expand (Zone v c) = Zone v' c'
      where
        v' = Set.union v c
        c' = (Set.filter (isOpen seed) (Set.fold (Set.union . surrounding) Set.empty c)) \\ v'
    surrounding (x, y) = Set.fromList [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]

isOpen :: Seed -> Location -> Bool
isOpen seed (x, y) = x >= 0 && y >= 0 && (even . popCount $ x * x + 3 * x + 2 * x * y + y + y * y + seed)

stepsToReach :: Seed -> Location -> Int
stepsToReach seed loc = fromMaybe (-1) . findIndex (Set.member loc) . map crest $ zones seed

numLocationsReached :: Seed -> Int -> Int
numLocationsReached seed steps = Set.size . (\m -> Set.union (visited m) (crest m)) $ (zones seed) !! steps

-- Final, top-level exports
day13 :: Int -> Int
day13 seed = stepsToReach seed (31, 39)

day13' :: Int -> Int
day13' seed = numLocationsReached seed 50

-- Input
run :: IO ()
run = do
    putStrLn "Day 13 results: "
    let input = 1364
    putStrLn $ "  " ++ show (day13 input)
    putStrLn $ "  " ++ show (day13' input)
