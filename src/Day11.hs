{-# LANGUAGE OverloadedStrings #-}
module Day11 (day11, day11', run) where

import Data.Either (rights)
import Text.Parsec ((<|>) , Parsec , ParseError)
import qualified Text.Parsec as P

-- Final, top-level exports
day11 :: String -> Int
day11 = undefined

day11' :: String -> Int
day11' = undefined

-- Input
run :: IO ()
run = do
    putStrLn "Day 11 results: "
    input <- readFile "inputs/day11.txt"
    putStrLn $ "  " ++ show (day11 input)
    putStrLn $ "  " ++ show (day11' input)
