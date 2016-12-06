module Day6 (day6, day6', run) where

import Data.List (group, sort, sortOn, transpose)

-- Final, top-level exports
day6 :: String -> String
day6 = map (head . head . sortOn (negate . length) . group . sort) . transpose . lines

day6' :: String -> String
day6' = map (head . head . sortOn length . group . sort) . transpose . lines

-- Input
run :: IO ()
run = do
    putStrLn "Day 6 results: "
    input <- readFile "inputs/day6.txt"
    putStrLn $ "  " ++ show (day6 input)
    putStrLn $ "  " ++ show (day6' input)
