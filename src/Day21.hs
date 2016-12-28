module Day21 where

import Data.Either (rights)
import Data.Maybe (fromJust)
import Data.Vector.Unboxed ((!), (//), Vector)
import qualified Data.Vector.Unboxed as Vector
import Text.Parsec ((<|>) , Parsec , ParseError)
import qualified Text.Parsec as P

type Password = Vector Char
data Instruction = Instruction
    { forwards  :: Password -> Password
    , backwards :: Password -> Password
    }

password :: String -> Password
password = Vector.fromList

parseInput :: String -> [Instruction]
parseInput = rights . map (P.parse parseInstruction "") . lines
  where
    parseInstruction = P.choice [parseSwap, parseRotate, parseReverse, parseMove]

number :: Parsec String () Int
number = read <$> P.many1 P.digit

-- Instructions
swapPositionInstruction :: Int -> Int -> Instruction
swapPositionInstruction x y = Instruction (swap x y) (swap x y)

swap :: Int -> Int -> Password -> Password
swap x y p = let a = p ! x; b = p ! y in p // [(x, b), (y, a)]

swapLetterInstruction :: Char -> Char -> Instruction
swapLetterInstruction a b = Instruction swap' swap'
  where
    swap' p = let
        Just x = Vector.findIndex (== a) p
        Just y = Vector.findIndex (== b) p
      in swap x y p

parseSwap = P.try (P.string "swap ") *> P.choice [parseSwapPosition, parseSwapLetter]
  where
    parseSwapPosition = do
        P.string "position "
        x <- number
        P.string " with position "
        y <- number
        return $ swapPositionInstruction x y
    parseSwapLetter = do
        P.string "letter "
        a <- P.anyChar
        P.string " with letter "
        b <- P.anyChar
        return $ swapLetterInstruction a b

-- Positive means rotate right, negative means rotate left
rotateInstruction :: Int -> Instruction
rotateInstruction x = Instruction (rotate x) (rotate (-x))

rotate :: Int -> Password -> Password
rotate x p = let l = Vector.length p in Vector.imap (\i _ -> p ! ((i - x) `mod` l)) p

parseRotate :: Parsec String () Instruction
parseRotate = P.try (P.string "rotate ") *> P.choice [parseRotateNumber, parseRotatePosition]
  where
    parseRotateNumber = do
        d <- (P.string "left " *> return (-1)) <|> (P.string "right " *> return 1)
        x <- number
        P.string " step" <* P.optional (P.string "s")
        return $ rotateInstruction (d * x)
    parseRotatePosition = do
        P.string "based on position of letter "
        a <- P.anyChar
        return $ rotatePositionInstruction a

rotatePositionInstruction :: Char -> Instruction
rotatePositionInstruction a = Instruction
    (\p -> let Just i = Vector.findIndex (== a) p in
        if i >= 4
        then forwards (rotateInstruction (i + 2)) p
        else forwards (rotateInstruction (i + 1)) p)
    (\p -> let Just i = Vector.findIndex (== a) p in
        if i == 0 then backwards (rotateInstruction 1) p else
            if odd i
            then backwards (rotateInstruction ((i + 1) `div` 2)) p
            else backwards (rotateInstruction ((i + 10) `div` 2)) p)

reverseInstruction :: Int -> Int -> Instruction
reverseInstruction x y = Instruction reverse reverse
  where
    reverse p = let
        (first, rest) = Vector.splitAt x p
        (middle, last) = Vector.splitAt (y - x + 1) rest
      in Vector.concat [first, Vector.reverse middle, last]

parseReverse :: Parsec String () Instruction
parseReverse = do
    P.try (P.string "reverse positions ")
    x <- number
    P.string " through "
    y <- number
    return $ reverseInstruction x y

moveInstruction :: Int -> Int -> Instruction
moveInstruction x y
    | x > y = Instruction
        (\p -> let
            first = Vector.take y p
            second = Vector.take (x - y) $ Vector.drop y p
            letter = Vector.singleton (p ! x)
            tail = Vector.drop (x + 1) p
          in Vector.concat [first, letter, second, tail])
        (\p -> forwards (moveInstruction y x) p)
    | otherwise = Instruction
        (\p -> let
            first = Vector.take x p
            letter = Vector.singleton (p ! x)
            second = Vector.take (y - x) $ Vector.drop (x + 1) p
            tail = Vector.drop (y + 1) p
          in Vector.concat [first, second, letter, tail])
        (\p -> forwards (moveInstruction y x) p)

parseMove :: Parsec String () Instruction
parseMove = do
    P.try (P.string "move position ")
    x <- number
    P.string " to position "
    y <- number
    return $ moveInstruction x y

scramble :: String -> [Instruction] -> String
scramble p = Vector.toList . go (Vector.fromList p)
  where
    go p' = foldl (\acc i -> (forwards i) acc) p'

unscramble :: String -> [Instruction] -> String
unscramble p = Vector.toList . go (Vector.fromList p)
  where
    go p' = foldr (\i acc -> (backwards i) acc) p'

-- Final, top-level exports
day21 :: String -> String
day21 = scramble "abcdefgh" . parseInput

day21' :: String -> String
day21' = unscramble "fbgdceah" . parseInput

-- Input
run :: IO ()
run = do
    putStrLn "Day 21 results: "
    input <- readFile "inputs/day21.txt"
    putStrLn $ "  " ++ show (day21 input)
    putStrLn $ "  " ++ show (day21' input)
