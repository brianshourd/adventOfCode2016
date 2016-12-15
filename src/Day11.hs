{-# LANGUAGE OverloadedStrings #-}
module Day11 (day11, day11', run, State(), buildState, Pair(..), minPathLength, destroysChips, parseInput, desiredState) where

import Data.Either (rights)
import Data.List (lookup, sort)
import Data.Maybe (fromJust)
import Data.Set ((\\), Set)
import qualified Data.Set as Set
import Text.Parsec ((<|>) , Parsec , ParseError)
import qualified Text.Parsec as P

-- data types
type Floor = Int
type Direction = Int -> Int
data Pair = Pair
    String -- name of the element
    Floor -- microchip floor number
    Floor -- generator floor number
    deriving (Show)
instance Eq Pair where
    (Pair _ m g) == (Pair _ m' g') = m == m' && g == g'
instance Ord Pair where
    compare (Pair _ m g) (Pair _ m' g') = compare (m, g) (m', g')
data State = State Floor [Pair] deriving (Eq, Ord, Show) -- use buildState
data Item = Microchip String | Generator String deriving (Eq, Ord, Show)
-- end data types

-- always construct a State with sorted pairs
buildState :: Floor -> [Pair] -> State
buildState f ps = State f (sort ps)

-- input parsing
parseInput :: String -> State
parseInput input = buildState 0 $ map makePair generators
  where
    makePair (Generator e, g) = Pair e (fromJust $ lookup (Microchip e) microchips) g
    generators = filter (isGenerator . fst) indexedItems
    microchips = filter (not . isGenerator . fst) indexedItems
    isGenerator (Generator _) = True
    isGenerator _ = False
    floors = rights . map (P.parse parseFloor "") . lines $ input
    indexedItems :: [(Item, Int)]
    indexedItems = concatMap (\(is, index) -> map (\i -> (i, index)) is) (zip floors [0..])

parseFloor :: Parsec String () [Item]
parseFloor = (P.count 4 (P.manyTill (P.noneOf " ") P.space)) *> (P.sepBy parseItem P.space)
  where
    parseItem = do
        (P.optional . P.try $ P.string "and ") >> P.string "a "
        item <- P.try parseGenerator <|> parseMicrochip
        P.optional (P.oneOf ",.")
        return item
    parseGenerator = Generator <$> (P.many1 P.lower <* P.string " generator")
    parseMicrochip = Microchip <$> (P.many1 P.lower <* P.string "-compatible microchip")
-- end input parsing

-- business logic
-- move a single Item in the given Direction
move1 :: Direction -> Item -> State -> State
move1 d i (State f ps) = buildState (d f) $ map (applyMoveToPair d i) ps

-- move two Items in the given Direction
move2 :: Direction -> Item -> Item -> State -> State
move2 d i1 i2 (State f ps) = buildState (d f) $ map (applyMoveToPair d i2 . applyMoveToPair d i1) ps

-- move an Item in the given Direction, if that Item belongs to this Pair
applyMoveToPair :: Direction -> Item -> Pair -> Pair
applyMoveToPair d i p = if match i p then doMove d i p else p
  where
    match (Microchip e) (Pair e' _ _) = e == e'
    match (Generator e) (Pair e' _ _) = e == e'
    doMove d (Microchip _) (Pair e m g) = Pair e (d m) g
    doMove d (Generator _) (Pair e m g) = Pair e m (d g)

destroysChips :: State -> Bool
destroysChips (State f ps) = any (\(Pair _ m g) -> floorHasGenerator m) $ separatePairs
  where
    separatePairs = filter (\(Pair _ m g) -> m /= g) ps
    floorHasGenerator floor = any (\(Pair _ m g) -> g == floor) ps

allMoves :: State -> Set State
allMoves s@(State f ps) = Set.fromList . filter (not . destroysChips) $
    [move1 d x s | d <- directions, x <- items] ++
    [move2 d x y s | d <- directions, x <- items, y <- items, x < y, allowedTogether x y]
  where
    directions = (if f < 3 then [(+1)] else []) ++ (if f > 0 then [(subtract 1)] else [])
    items = generators ++ microchips
    generators = map (\(Pair e _ _) -> Generator e) . filter (\(Pair _ _ g) -> g == f) $ ps
    microchips = map (\(Pair e _ _) -> Microchip e) . filter (\(Pair _ m _) -> m == f) $ ps
    allowedTogether (Microchip e) (Generator e') = e == e'
    allowedTogether (Generator e) (Microchip e') = e == e'
    allowedTogether _ _ = True

-- calculates the desired final state (everything on the top floor)
desiredState :: State -> State
desiredState (State f ps) = State 3 $ map (\(Pair e _ _) -> Pair e 3 3) ps
-- end business logic

-- simplified bi-directional dijkstra's algorithm
data SearchZone a = SearchZone
    (Set a) -- everywhere that has been visited
    (Set a) -- the "crest" of what has been visited - subset of above

expandSearchZone :: SearchZone State -> SearchZone State
expandSearchZone (SearchZone v c) = SearchZone v' c'
  where
    v' = Set.union v c'
    c' = (Set.fold (Set.union . allMoves) Set.empty c) \\ v

zonesIntersect :: SearchZone State -> SearchZone State -> Bool
zonesIntersect (SearchZone v _) (SearchZone v' _) = not . Set.null $ Set.intersection v v'

minPathLength :: (State, State) -> Int
minPathLength (s, s') = minPathLength' (newSearchZone s, newSearchZone s') 0
  where
    newSearchZone state = let set = Set.singleton state in SearchZone set set
    minPathLength' (sz, sz') n
        | zonesIntersect sz sz' = n
        | otherwise = minPathLength' (sz', expandSearchZone sz) (n + 1)
-- end dijkstra's algorithm

-- Final, top-level exports
day11 :: String -> Int
day11 input = minPathLength (startState, desiredState startState)
  where
    startState = parseInput input

day11' :: String -> Int
day11' input = minPathLength (startState, desiredState startState)
  where
    (State 0 ps) = parseInput input
    startState = buildState 0 $ (Pair "elerium" 0 0) : (Pair "dilithium" 0 0) : ps

-- Input
run :: IO ()
run = do
    putStrLn "Day 11 results: "
    input <- readFile "inputs/day11.txt"
    putStrLn $ "  " ++ show (day11 input)
    putStrLn $ "  " ++ show (day11' input)
