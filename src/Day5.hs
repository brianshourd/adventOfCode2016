module Day5 (day5, day5', run, startsWithZeros, applyHash) where

import Crypto.Hash.MD5 (hash)
import Data.ByteString.Base16 (encode)
import qualified Data.ByteString.Char8 (pack, unpack)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS (index, isPrefixOf, pack)
import Data.List (elemIndex, nub, scanl)
import Data.Map.Strict (Map, (!), insert, member)
import qualified Data.Map.Strict as Map (empty)
import Data.Word (Word8)

applyHash :: String -> Int -> ByteString
applyHash prefix x = hash . Data.ByteString.Char8.pack $ prefix ++ show x

startsWithZeros :: ByteString -> Bool
startsWithZeros b = zeros `BS.isPrefixOf` b && ((BS.index b 2) < 16)
  where
    zeros = BS.pack [0, 0]

-- All base16 representations of hashes whose first 5 characters are '0'
interestingHashes :: String -> [String]
interestingHashes prefix = map (Data.ByteString.Char8.unpack . encode) . filter startsWithZeros . map (applyHash prefix) $ [1..]

newtype PartiallyDecrypted = PartiallyDecrypted (Map Int Char) deriving (Eq, Ord)
instance Show PartiallyDecrypted where
    show (PartiallyDecrypted m) = map charOrUnderscore [0..7]
      where
        charOrUnderscore i = if (member i m) then m ! i else '_'

isFullyDecrypted :: PartiallyDecrypted -> Bool
isFullyDecrypted (PartiallyDecrypted m) = all (\i -> member i m) [0..7]

decryptStep :: PartiallyDecrypted -> String -> PartiallyDecrypted
decryptStep p@(PartiallyDecrypted m) s = if pos < 8 then (PartiallyDecrypted $ insertIgnore pos char m) else p
  where
    pos = maybe 9 id $ elemIndex (s !! 5) "01234567"
    char  = s !! 6
    insertIgnore position character map = if member position map then map else insert position character map

-- Expects to be fed results of interestingHashes, so that interestingHashes can
-- be cached between parts
day5Impl :: [String] -> String
day5Impl = take 8 . map (!! 5)

day5Impl' :: [String] -> String
day5Impl' = show . head . dropWhile (not . isFullyDecrypted) . scanl decryptStep (PartiallyDecrypted Map.empty)

-- Final, top-level exports
day5 :: String -> String
day5 = day5Impl . interestingHashes

day5' :: String -> String
day5' = day5Impl' . interestingHashes

-- Input
run :: IO ()
run = do
    -- The actual solution here is commented out, because it takes ~45 seconds
    --putStrLn "Day 5 results: "
    --let input = "ojvtpuvg"
    --let hashes = interestingHashes input
    --putStrLn $ "  " ++ show (day5Impl hashes)
    --putStrLn $ "  " ++ show (day5Impl' hashes)
    putStrLn "Day 5 results (cached): "
    putStrLn $ "  " ++ show "4543c154"
    putStrLn $ "  " ++ show "1050cbbd"
