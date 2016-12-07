{-# LANGUAGE OverloadedStrings #-}
module Day7 (day7, day7', run) where

import Control.Monad (liftM2)
import Data.Attoparsec.Text (Parser(..), char, parseOnly, many1, takeTill)
import Data.Either (rights)
import Data.List (intersect)
import Data.Text (pack, unpack)

data IP = IP
    [String] -- regular sequences
    [String] -- hypernet sequences
    deriving (Eq, Ord, Show)

parseLine :: String -> Either String IP
parseLine = parseOnly parseIP . pack
  where
    parseIP = do
        (reg, hyper) <- fmap unzip . many1 $
            liftM2 (,) parseSequence (char '[' *> parseSequence <* char ']')
        final <- parseSequence
        return $ IP (reg ++ [final]) hyper
    parseSequence = fmap unpack $ takeTill (\x -> x == '[' || x == ']')

supportsTLS :: IP -> Bool
supportsTLS (IP reg hyper) = (any containsABBA reg) && (all (not . containsABBA) hyper)

containsABBA :: String -> Bool
containsABBA s@(a:b:c:d:_) = (a == d && b == c && a /= b) || (containsABBA $ tail s)
containsABBA _ = False

supportsSSL :: IP -> Bool
supportsSSL (IP reg hyper) = not . null $ intersect abas babs
  where
    abas = concatMap getABAs reg
    babs = map flipBAB $ concatMap getABAs hyper
    flipBAB (a:b:_:[]) = (b:a:b:[])

getABAs :: String -> [String]
getABAs = getABAs' []
  where
    getABAs' :: [String] -> String -> [String]
    getABAs' known s@(a:b:c:_) = getABAs' known' (tail s)
      where
        known' = if (a == c && a /= b) then (a:b:c:[]):known else known
    getABAs' known _ = known

-- Final, top-level exports
day7 :: String -> Int
day7 = length . filter supportsTLS . rights . map parseLine . lines

day7' :: String -> Int
day7' = length . filter supportsSSL . rights . map parseLine . lines

-- Input
run :: IO ()
run = do
    putStrLn "Day 7 results: "
    input <- readFile "inputs/day7.txt"
    putStrLn $ "  " ++ show (day7 input)
    putStrLn $ "  " ++ show (day7' input)
