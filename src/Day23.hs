module Day23 where

import Assembunny

-- Final, top-level exports
day23 :: String -> Int
day23 = readRegister 'a' . evaluate . writeRegister 'a' 7 . start . parseInput

day23' :: String -> Int
day23' = readRegister 'a' . evaluate . writeRegister 'a' 12 . start . parseInput

-- Input
run :: IO ()
run = do
    putStrLn "Day 23 results: "
    input <- readFile "inputs/day23.txt"
    putStrLn $ "  " ++ show (day23 input)
    putStrLn $ "  " ++ show (day23' input)
