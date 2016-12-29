module Assembunny (Execution(), Expr(..), Value(..), Register(..), parseInput, start, readRegister, writeRegister, evaluate, tempInput, evaluate1) where

import Data.Either (rights)
import Data.Map ((!), Map)
import qualified Data.Map as Map
import Data.Vector ((//), (!?), Vector)
import qualified Data.Vector as Vector
import Text.Parsec ((<|>), Parsec)
import qualified Text.Parsec as P

data Value = Literal Int | Reference Register deriving (Eq, Ord, Show)
newtype Register = Register Char deriving (Eq, Ord, Show)
data Expr =
    IncExpr Register
    | DecExpr Register
    | ToggleExpr Register
    | JumpExpr Value Value
    | CopyExpr Value Value
    deriving (Eq, Ord, Show)
type Instructions = Vector Expr

parseInput :: String -> [Expr]
parseInput = rights . map (P.parse parseExpr "") . lines

parseExpr :: Parsec String () Expr
parseExpr = P.choice [parseIncExpr, parseDecExpr, parseToggleExpr, parseJumpExpr, parseCopyExpr]
  where
    parseIncExpr = IncExpr <$> ((P.try $ P.string "inc ") *> parseRegister)
    parseDecExpr = DecExpr <$> ((P.try $ P.string "dec ") *> parseRegister)
    parseToggleExpr = ToggleExpr <$> ((P.try $ P.string "tgl ") *> parseRegister)
    parseJumpExpr = JumpExpr <$> ((P.try $ P.string "jnz ") *> parseValue <* P.spaces) <*> parseValue
    parseCopyExpr = CopyExpr <$> ((P.try $ P.string "cpy ") *> parseValue <* P.spaces) <*> parseValue
    parseRegister = Register <$> P.oneOf "abcd"
    parseValue = (P.try (Literal <$> parseInt)) <|> (Reference <$> parseRegister)

parseInt :: Parsec String () Int
parseInt = do
    minus <- P.option "" (P.string "-")
    digits <- P.many1 P.digit
    return $ read (minus ++ digits)

data Execution = Execution
    { lineNum :: Int
    , registers :: Map Register Int
    , instructions :: Vector Expr
    } deriving (Eq, Ord, Show)

start :: [Expr] -> Execution
start = Execution 0 (Map.fromList [(r, 0) | r <- map Register "abcd"]) . Vector.fromList

evaluate :: Execution -> Execution
evaluate e@(Execution l rs is) = case is !? l of
    Just expr -> evaluate (evaluate1 expr e)
    Nothing -> e

evaluate1 :: Expr -> Execution -> Execution
evaluate1 (IncExpr r) e = incLineNum $ setValue e r ((getValue e (Reference r)) + 1)
evaluate1 (DecExpr r) e = incLineNum $ setValue e r ((getValue e (Reference r)) - 1)
evaluate1 (ToggleExpr r) e = let l = (lineNum e) + (getValue e (Reference r)) in
    incLineNum $ case (instructions e) !? l of
        Just expr -> e { instructions = (instructions e) // [(l, toggle expr)] }
        Nothing -> e
evaluate1 (JumpExpr v v') e = if getValue e v > 0 then e { lineNum = (lineNum e + (getValue e v')) } else incLineNum e
evaluate1 (CopyExpr v (Reference r)) e = incLineNum $ setValue e r (getValue e v)
evaluate1 (CopyExpr v (Literal _)) e = incLineNum e

toggle :: Expr -> Expr
toggle (IncExpr r) = DecExpr r
toggle (DecExpr r) = IncExpr r
toggle (ToggleExpr r) = IncExpr r
toggle (JumpExpr v v') = CopyExpr v v'
toggle (CopyExpr v v') = JumpExpr v v'

incLineNum :: Execution -> Execution
incLineNum e = e { lineNum = (lineNum e) + 1 }

getValue :: Execution -> Value -> Int
getValue e (Literal n) = n
getValue e (Reference r) = (registers e) ! r

setValue :: Execution -> Register -> Int -> Execution
setValue e r n = e { registers = Map.insert r n (registers e) }

readRegister :: Char -> Execution -> Int
readRegister c e = (registers e) ! (Register c)

writeRegister :: Char -> Int -> Execution -> Execution
writeRegister c n e = setValue e (Register c) n

tempInput :: String
tempInput = unlines
    [ "cpy 2 a"
    , "tgl a"
    , "tgl a"
    , "tgl a"
    , "cpy 1 a"
    , "dec a"
    , "dec a"
    ]

