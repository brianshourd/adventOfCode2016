{-# LANGUAGE OverloadedStrings #-}
module Day4 (day4, day4', run, decrypt, RoomData(..)) where

import Data.Char (chr, isLower, ord)
import Data.Either (rights)
import Data.List (group, sort, sortOn)
import Text.Parsec
    ( ParseError
    , between
    , char
    , digit
    , endBy1
    , lower
    , many1
    , parse
    )

data RoomData = RoomData [String] Int String deriving (Eq, Ord, Show)

parseRoomData :: String -> Either ParseError RoomData
parseRoomData = parse parseImpl ""
  where
    parseImpl = do
        encryptedName <- endBy1 (many1 lower) (char '-')
        sectorId <- read <$> many1 digit
        checksum <- between (char '[') (char ']') (many1 lower)
        return $ RoomData encryptedName sectorId checksum

isValid :: RoomData -> Bool
isValid (RoomData name _ checksum) = (take 5 . map head . sortOn (negate . length) . group . sort . concat $ name) == checksum

decrypt :: RoomData -> String
decrypt (RoomData name sectorId _) = unwords . map (map (toChar . shift . toInt)) $ name
  where
    toInt = subtract 97 . ord
    toChar = chr . (+) 97
    shift x = (x + sectorId) `mod` 26

sectorId :: RoomData -> Int
sectorId (RoomData _ s _) = s

-- Final, top-level exports
day4 :: String -> Int
day4 = sum . map (\(RoomData _ i _) -> i) . filter isValid . rights . map parseRoomData . lines

day4' :: String -> Int
day4' = sectorId . head . filter ((==) "northpole object storage" . decrypt) . filter isValid . rights . map parseRoomData . lines

-- Read from input file
run :: IO ()
run = do
    putStrLn "Day 4 results: "
    input <- readFile "inputs/day4.txt"
    putStrLn $ "  " ++ show (day4 input)
    putStrLn $ "  " ++ show (day4' input)
