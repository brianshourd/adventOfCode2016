{-# LANGUAGE OverloadedStrings #-}
module Day12 where

import Assembunny

-- Final, top-level exports
day12 :: String -> Int
day12 = readRegister 'a' . evaluate . start . parseInput

day12' :: String -> Int
day12' = readRegister 'a' . evaluate . writeRegister 'c' 1 . start . parseInput

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
