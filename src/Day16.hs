module Day16 where

import Data.Bits
import Data.List.Split (chunksOf)

zeroDragon :: [Bool]
zeroDragon = map (zeroDragon' False) [1..]
  where
    zeroDragon' :: Bool -> Int -> Bool
    zeroDragon' shouldFlip 0 = shouldFlip
    zeroDragon' shouldFlip n = let k = 2^(logBase2 n) in
        if n == k then xor shouldFlip False else zeroDragon' (not shouldFlip) (2 * k - n)

dragonLevel :: [Bool] -> Int -> [Bool]
dragonLevel s 0 = s ++ (concat $ zipWith (:) zeroDragon (concat $ repeat [s', s]))
  where
    s' = map not $ reverse s
dragonLevel s n = map go . chunksOf 2 $ dragonLevel s (n - 1)
  where
    go (x : y : _) = x == y

logBase2 :: Int -> Int
logBase2 k = finiteBitSize k - 1 - countLeadingZeros k

-- Final, top-level exports
day16 :: String -> Int -> String
day16 input n = toString . take (length input) $ dragonLevel (fromString input) (logBase2 (n `div` (length input)))

toString = map (\b -> if b then '1' else '0')
fromString = map (\c -> if c == '1' then True else False)

-- Input
run :: IO ()
run = do
    putStrLn "Day 16 results: "
    let input = "10001001100000001"
    putStrLn $ "  " ++ show (day16 input 272)
--    putStrLn $ "  " ++ show (day16 input 35651584)
    putStrLn $ "  \"10100001110101001\" (cached)"
