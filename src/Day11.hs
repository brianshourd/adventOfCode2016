{-# LANGUAGE OverloadedStrings #-}
-- |This module solves the problem presented as
-- <http://adventofcode.com/2016/day/11 day 11 of the 2016 advent of code>. The
-- code is also viewable
-- <https://github.com/brianshourd/adventOfCode2016/blob/master/src/Day11.hs on github>.
module Day11 (
-- * Maze-solving algorithm
-- |First, let's talk about solving a maze. A plain-old, two-dimensional maze.
-- One where you need to know the minimum number of steps required to get from
-- some start location to some end location. One way to solve such a problem is
-- to use a simplified form of Dijkstra's Algorithm (the maze is a uniformly
-- weighted, undirected graph where the nodes are open spaces and two nodes are
-- connected with an edge if and only if they are adjacent and not separated by
-- a wall).
--
-- Suppose that you know all of the locations in the maze that you can reach in
-- n steps. Then if you loop over each each location in that set and look at all
-- adjacent locations, every /new/ location that you find is reachable in (n+1)
-- steps. As long as you can always find all adjacent locations to a given
-- location, you can continue doing this.
--
-- To picture this, imagine that the below is a portion of a maze, where @' '@
-- represents an unexplored location, @'*'@ represents an explored loctaion, and
-- @'#'@ represents a wall. Suppose we start with something like this:
--
-- >    #  # #
-- > # ## ## #
-- > # #  #   
-- > #   *# # 
-- >   #    # 
-- >  ## #### 
--
-- This means that we have only explored a single location (the starting
-- location). If we add all locations adjacent to locations that we've already
-- explored, that expands our search zone to:
--
-- >    #  # #
-- > # ## ## #
-- > # # *#   
-- > #  **# # 
-- >   # *  # 
-- >  ## #### 
--
-- These are all of the locations that can be reached in a single move. We can
-- expand again:
--
-- >    #  # #
-- > # ##*## #
-- > # #**#   
-- > # ***# # 
-- >   #*** # 
-- >  ## #### 
--
-- In fact, it becomes handy to keep track of more than just the locations that
-- we've visited. We can keep track of the "crest" of these visited locations
-- separately. Let's re-code the above mazes so that @'*'@ represents the crest
-- of visited locations (i.e. locations that we only just arrived at), and @'.'@
-- represents previously-visited locations.
--
-- >    #  # #  |     #  # #  |     #  # #  |   # #* # #
-- > # ## ## #  |  # ## ## #  |  # ##*## #  |  # ##.## #
-- > # #  #     |  # # *#     |  # #*.#     |  # #..#   
-- > #   *# #   |  #  *.# #   |  # *..# #   |  #*...# # 
-- >   #    #   |    # *  #   |    #*.* #   |    #...*# 
-- >  ## ####   |   ## ####   |   ## ####   |   ##*#### 
--
-- This has the advantage that when we expand our search zone, we don't need to
-- look at /every/ location that we've visited, just the ones on the crest.
--
-- Then to find out how many steps it takes to get from our start location to
-- some final location, we just see how many times we have to do this "expand
-- the search zone" process before our desired location is inside the search
-- zone.
--
-- In fact, we can do better than that. Instead of having only one search zone
-- that continues to get bigger and bigger, we can have two search zones. One
-- searches around the start location, and one searches around the end location.
-- When the zones intersect, our algorithm has completed.
    minPathLength
    , Location(..)
    -- * Side note
    -- |Incidentally, it is possible to define a 'Location' to use this exact
    -- same code to solve <http://adventofcode.com/2016/day/13 problem 13>. This
    -- exercise is left to the reader.

    -- * Business logic
    -- |Okay, how does that maze-solving algorithm help us to solve /this/
    -- problem. It's not a maze at all.
    --
    -- ...or is it?
    --
    -- Lots of things are actually mazes! We just have to re-categorize what we
    -- think of as a 'Location'. Remember, to be a 'Location', all that we need
    -- is to be able to find adjacent locations. If we think of the current
    -- state (position of the elevator and positions of all of the microchips
    -- and generators) as a 'Location', then the concept of adjacent locations
    -- is simply "all of the ways that we could move the elevator a single floor
    -- without frying any microchips".
    --
    -- We just have to implement two things:
    --
    --   * Define this 'State' data type
    --   * Implement the 'adjacentLocations' function for 'State'
    , Floor(..)
    , Pair(..)
    , State(..)
    , buildState
    , adjacentStates
    , destroysChips
    -- * Actually solve the problem
    , day11
    , day11'
    , run
    , desiredState
    -- * Input parsing
    , parseInput
    ) where

import Data.Either (rights)
import Data.List (lookup, sort)
import Data.Maybe (fromJust)
import Data.Set ((\\), Set)
import qualified Data.Set as Set
import Text.Parsec ((<|>) , Parsec , ParseError)
import qualified Text.Parsec as P

-- |Here's the data structure that we'll use to represent a search zone. This
-- data structure is parameterized over type @a@, which is the type of our
-- location. In the maze example above, @a@ would be the type @(Int, Int)@.
data SearchZone a = SearchZone
    { visited :: (Set a) -- ^ The set of all previously visited locations
    , crest :: (Set a) -- ^ The most-recently-visited locations, subset of 'visited'
    }

-- |And this is the typeclass that we'll use to power our search. In order to be
-- considered a 'Location', a type just needs to be able to list adjacent
-- locactions.
--
-- This also has the type constraint @(Ord a)@, meaning that locations must be
-- able to be ordered. The actual order isn't important, this is mostly a
-- side-effect of the fact that we are using @Set@s to hold these items (and
-- most @Set@ functionality requires @Ord@).
class Ord a => Location a where
    adjacentLocations :: a -> Set a

-- |The implementation of the algorithm. Note that it works on any type that
-- implements 'Location'. Note also that the public signature does not include
-- any reference to 'SearchZone' - this is just an implementation detail.
minPathLength :: Location a => (a, a) -> Int
minPathLength (l, l') = minPathLength' (newSearchZone l, newSearchZone l') 0
  where
    newSearchZone location = let set = Set.singleton location in SearchZone set set

    minPathLength' :: Location a => (SearchZone a, SearchZone a) -> Int -> Int
    minPathLength' (sz, sz') n
        | zonesIntersect sz sz' = n
        -- Note that we flip sz and sz', since we always expand the first
        -- element in our tuple
        | otherwise = minPathLength' (sz', expandSearchZone sz) (n + 1)

    zonesIntersect :: Location a => SearchZone a -> SearchZone a -> Bool
    zonesIntersect (SearchZone v _) (SearchZone v' _) = not . Set.null $ Set.intersection v v'

-- |This is the function used to expand a search zone. Note that it has
-- constraint @(Location a)@
expandSearchZone :: (Location a) => SearchZone a -> SearchZone a
expandSearchZone (SearchZone v c) = SearchZone v' c'
  where
    v' = Set.union v c'
    c' = (Set.fold (Set.union . adjacentLocations) Set.empty c) \\ v

-- |We will represent a floor number with an 'Int', but we use a type alias for
-- clarity
type Floor = Int

-- |To represent a microchip and generator pair, we use the 'Pair' data type.
-- The first argument is the name of the element, the second is the floor number
-- of the microchip for that element, and the third is the floor number of the
-- generator for that element.
--
-- Ex. We can represent that the plutonium generator is on the second floor and
-- the plutonium-compatible microchip is on the third floor with
--
-- > Pair "plutonium" 2 1
data Pair = Pair
    String -- Name of the element
    Floor -- Microchip floor number
    Floor -- Generator floor number
    deriving (Show)

-- |Here's where things get interesting. We don't really care about the name of
-- the element when we're comparing pairs. For our purposes, having a plutonium
-- generator-microchip pair on floor 1 and a uranium generator-microchip pair on
-- floor 2 is exactly the same as having a uranium generator-microchip pair on
-- floor 1 and a plutonium generator-microchip pair on floor 2. The options we
-- can take in either case are completely equivalent. So we tell the
-- type system to implement equality and comparison for 'Pair's by ignoring the
-- name of the element.
instance Eq Pair where
    (Pair _ m g) == (Pair _ m' g') = m == m' && g == g'
instance Ord Pair where
    compare (Pair _ m g) (Pair _ m' g') = compare (m, g) (m', g')

-- | The 'State' data type describes the position of the elevator and the
-- positions of the various elements. For example, the sample input:
--
-- > The first floor contains a hydrogen-compatible microchip and a lithium-compatible microchip.
-- > The second floor contains a hydrogen generator.
-- > The third floor contains a lithium generator.
-- > The fourth floor contains nothing relevant.
--
-- is represented by
--
-- > State 0 [ Pair "hydrogen" 0 1, Pair "lithium" 0 2 ]
data State = State Floor [Pair] deriving (Eq, Ord, Show)

instance Location State where
    adjacentLocations = adjacentStates

-- |One problem with representing 'State' with a list of 'Pair's is that two
-- 'State's with the same pairs in different orders would not be considered
-- equal. We could solve this by alwasy sorting their 'Pair' lists when checking
-- for equality, but a much more efficient method is to always sort the 'Pair's
-- at construction time. `buildState` is just such a constructor
buildState :: Floor -> [Pair] -> State
buildState f ps = State f (sort ps)

-- |To implement 'adjacentStates', as well as when parsing input, it is
-- sometimes needed to represent an item alone
data Item = Microchip String | Generator String deriving (Eq, Ord, Show)

-- |This is the main implementation requirement, calculating all possible
-- (valid) states that can be reached in a single elevator ride from a given
-- state.
adjacentStates :: State -> Set State
adjacentStates s@(State f ps) = Set.fromList . filter (not . destroysChips) $
    [move1 d x s | d <- directions, x <- items] ++
    [move2 d x y s | d <- directions, x <- items, y <- items, x < y, allowedTogether x y]
  where
    directions = (if f < 3 then [(+1)] else []) ++ (if f > 0 then [(subtract 1)] else [])

    items = generators ++ microchips
    generators = map (\(Pair e _ _) -> Generator e) . filter (\(Pair _ _ g) -> g == f) $ ps
    microchips = map (\(Pair e _ _) -> Microchip e) . filter (\(Pair _ m _) -> m == f) $ ps

    allowedTogether :: Item -> Item -> Bool
    -- We cannot put a microchip and a generator into the elevator together
    -- unless they are a matched pair (otherwise, the generator will fry the
    -- microchip)
    allowedTogether (Microchip e) (Generator e') = e == e'
    allowedTogether (Generator e) (Microchip e') = e == e'
    allowedTogether _ _ = True

-- The following are all of the helper functions for 'adjacentStates'
type Direction = Int -> Int

-- |Move a single 'Item' in the given 'Direction', moving the elevator as well
move1 :: Direction -> Item -> State -> State
move1 d i (State f ps) = buildState (d f) $ map (applyMoveToPair d i) ps

-- |Move two 'Items' in the given 'Direction', moving the elevator as well
move2 :: Direction -> Item -> Item -> State -> State
move2 d i1 i2 (State f ps) = buildState (d f) $ map (applyMoveToPair d i2 . applyMoveToPair d i1) ps

-- |Move an 'Item' in the given 'Direction', if that 'Item' belongs to this
-- 'Pair'
applyMoveToPair :: Direction -> Item -> Pair -> Pair
applyMoveToPair d i p = if match i p then doMove d i p else p
  where
    match (Microchip e) (Pair e' _ _) = e == e'
    match (Generator e) (Pair e' _ _) = e == e'
    doMove d (Microchip _) (Pair e m g) = Pair e (d m) g
    doMove d (Generator _) (Pair e m g) = Pair e m (d g)

-- |Check whether or not a 'State' will result in one or more destroyed chips
destroysChips :: State -> Bool
destroysChips (State f ps) = any (\(Pair _ m g) -> floorHasGenerator m) $ separatePairs
  where
    separatePairs = filter (\(Pair _ m g) -> m /= g) ps
    floorHasGenerator floor = any (\(Pair _ m g) -> g == floor) ps

-- |The 'parseInput' function handles constructing an initial state from a
-- description. Internally, it uses 'Parsec', which is a great library that I'm
-- not going to go into now
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

-- |Given an input state, we want to know what the desired final state looks
-- like (everything on the top floor)
desiredState :: State -> State
desiredState (State f ps) = State 3 $ map (\(Pair e _ _) -> Pair e 3 3) ps

-- |Once we have defined 'State' to be an instance of 'Location State', we can
-- use our generic maze-solving algorithm to solve it.
day11 :: String -> Int
day11 input = minPathLength (startState, endState)
  where
    startState = parseInput input
    endState = desiredState startState

-- |The only difference between parts 1 and 2 is that in part 2, we must modify
-- the state we get from the input to have a couple of extra 'Pair's. Otherwise,
-- we still use the 'minPathLength' function to solve it.
day11' :: String -> Int
day11' input = minPathLength (startState, endState)
  where
    (State 0 ps) = parseInput input
    startState = buildState 0 $ (Pair "elerium" 0 0) : (Pair "dilithium" 0 0) : ps
    endState = desiredState startState

-- Input
run :: IO ()
run = do
    putStrLn "Day 11 results: "
    input <- readFile "inputs/day11.txt"
    putStrLn $ "  " ++ show (day11 input)
    putStrLn $ "  " ++ show (day11' input)
