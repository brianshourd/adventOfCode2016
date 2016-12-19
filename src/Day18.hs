module Day18 where

import Data.Maybe (mapMaybe)

newtype Floor = Floor [[Bool]] deriving (Eq, Ord)
-- True means trapped, False means free
instance Show Floor where
    show (Floor rs) = unlines $ map (map fromBool) rs

toBool :: Char -> Maybe Bool
toBool '^' = Just True
toBool '.' = Just False
toBool _ = Nothing

fromBool :: Bool -> Char
fromBool t = if t then '^' else '.'

buildFloor :: Int -> String -> Floor
buildFloor numRows row0 = Floor . take numRows . iterate next $ mapMaybe toBool row0
  where
    next f = let f' = (False : f) ++ [False] in
        zipWith3 isTrap f' (tail f') (tail $ tail f')
    isTrap l c r = ((c == l) || (c == r)) && (not (l == r))

countSafeTiles :: Floor -> Int
countSafeTiles (Floor rs) = length . filter not $ concat rs

-- Final, top-level exports
day18 :: String -> Int
day18 = countSafeTiles . buildFloor 40

day18' :: String -> Int
day18' = countSafeTiles . buildFloor 400000

-- Input
run :: IO ()
run = do
    putStrLn "Day 18 results: "
    input <- readFile "inputs/day18.txt"
    putStrLn $ "  " ++ show (day18 input)
    putStrLn $ "  20005203 (cached)"
