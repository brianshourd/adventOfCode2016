module Day14 (day14, day14', run, normalHash, stretchedHash) where

import Crypto.Hash.MD5 (hash)
import Data.ByteString.Base16 (encode)
import qualified Data.ByteString.Char8 as Char8
import Data.ByteString (ByteString)
import Data.List (intersect, tails)
import Data.Maybe (mapMaybe)
import Text.Regex.PCRE

data Potential = Potential
    { index :: Int
    , threes :: String
    , fives :: String
    } deriving (Eq, Ord, Show)

normalHash :: String -> Int -> ByteString
normalHash prefix x = encode . hash . Char8.pack $ prefix ++ show x

stretchedHash :: String -> Int -> ByteString
stretchedHash prefix x = (!! 2017) . iterate (encode . hash) $ (Char8.pack $ prefix ++ show x)

potentials :: (Int -> ByteString) -> [Potential]
potentials h = mapMaybe buildPotential [0..]
  where
    buildPotential :: Int -> Maybe Potential
    buildPotential i = let hash = h i in case getThrees hash of
        [] -> Nothing
        threes -> Just $ Potential i threes (getFives hash)
    getThrees :: ByteString -> String
    getThrees input = map Char8.head (getAllTextMatches $ input =~ "(.)\\1{2,}" :: [ByteString])
    getFives :: ByteString -> String
    getFives input = map Char8.head (getAllTextMatches $ input =~ "(.)\\1{4,}" :: [ByteString])

keyIndexes :: (Int -> ByteString) -> [Int]
keyIndexes = map (index . head) . filter isKey . tails . potentials
  where
    isKey :: [Potential] -> Bool
    isKey ((Potential i (t:_) _) : ps) = any (not . null . filter (== t) . fives) $ takeWhile ((< (i + 1000)) . index) ps

-- Final, top-level exports
day14 :: String -> Int
day14 = head . drop 63 . keyIndexes . normalHash

day14' :: String -> Int
day14' = head . drop 63 . keyIndexes . stretchedHash

-- Input
run :: IO ()
run = do
    putStrLn "Day 14 results: "
    let input = "yjdafjpo"
    putStrLn $ "  " ++ show (day14 input)
--    putStrLn $ "  " ++ show (day14' input)
    putStrLn $ "  22045 (cached)"
