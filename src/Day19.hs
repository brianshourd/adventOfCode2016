{-# LANGUAGE ViewPatterns, PatternSynonyms #-}
module Day19 where

import qualified Data.List as List
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq

-- Pattern matching for sequences - see http://stackoverflow.com/a/31106916/1853862
pattern Empty   <- (Seq.viewl -> Seq.EmptyL)  where Empty = Seq.empty
pattern x :< xs <- (Seq.viewl -> x Seq.:< xs) where (:<)  = (Seq.<|)
pattern xs :> x <- (Seq.viewr -> xs Seq.:> x) where (:>)  = (Seq.|>)

-- Final, top-level exports
day19 :: Int -> Int
day19 start = (\(_, _, o) -> o) . head . dropWhile (\(n, _, _) -> n > 1) $ iterate doRound (start, 1, 1)
  where
    doRound (n, i, o) = (n `div` 2, i + 1, if odd n then o + 2^i else o)

day19' :: Int -> Int
day19' start = go (makeCircle' start) (odd start)
  where
    go :: Seq Int -> Bool -> Int
    go Empty        _     = undefined
    go (x :< Empty) _     = x
    go s            isOdd
        | isOdd     = go (pop s) (not isOdd)
        | otherwise = go (pop $ cycle s) (not isOdd)
    cycle (x :< xs) = xs :> x
    pop   (x :< xs) = xs

makeCircle' :: Int -> Seq Int
makeCircle' start = let (a, b) = List.splitAt middle [1..start] in Seq.fromList (b ++ a)
  where
    middle = let half = start `div` 2 in if even start then half - 1 else half

-- Input
run :: IO ()
run = do
    putStrLn "Day 19 results: "
    let input = 3001330
    putStrLn $ "  " ++ show (day19 input)
    putStrLn $ "  " ++ show (day19' input)
