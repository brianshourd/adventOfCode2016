module Day17 where

import Crypto.Hash.MD5 (hash)
import Data.ByteString.Base16 (encode)
import qualified Data.ByteString.Char8 as Char8
import Data.ByteString (ByteString)
import Data.Function (on)
import Data.Foldable (find, maximumBy)
import Data.Set ((\\), Set)
import qualified Data.Set as Set

data Path = Path
    { passcode :: String
    , path :: String
    , pathLength :: Int
    , position :: (Int, Int)
    } deriving (Eq, Ord, Show)

newPath :: String -> Path
newPath pass = Path pass "" 0 (0, 0)

goal :: (Int, Int)
goal = (3, 3)

-- Outputs a bytestring in lowercase hex format
hashPath :: Path -> String
hashPath (Path a b _ _) = Char8.unpack . encode . hash . Char8.pack $ a ++ b

expand :: Path -> Set Path
expand p@(Path pass path l pos)
    | pos == goal = Set.empty
    | otherwise = Set.map moveDirection $ legalDirections p
  where
    moveDirection c = Path pass (path ++ [c]) (l + 1) (applyMove c pos)

legalDirections :: Path -> Set Char
legalDirections p = Set.fromList . filter onGrid . map fst . filter (openDoor . snd) . zip "UDLR" . take 4 $ hashPath p
  where
    openDoor c = Set.member c openLetters
    openLetters = Set.fromList "bcdef"
    onGrid c = let (x, y) = applyMove c (position p) in
        x <= 3 && x >= 0 && y <= 3 && y >= 0

applyMove :: Char -> (Int, Int) -> (Int, Int)
applyMove 'U' (x, y) = (x, y - 1)
applyMove 'D' (x, y) = (x, y + 1)
applyMove 'L' (x, y) = (x - 1, y)
applyMove 'R' (x, y) = (x + 1, y)
applyMove _ p = p

data SearchZone = SearchZone
    { visited :: Set Path
    , crest :: Set Path
    } deriving (Eq, Ord, Show)

expandSearchZone :: SearchZone -> SearchZone
expandSearchZone (SearchZone v c) = SearchZone v' c'
  where
    v' = Set.union v c'
    c' = (Set.fold (Set.union . expand) Set.empty c) \\ v

findShortestPath :: String -> Path
findShortestPath passcode = let start = (Set.singleton $ newPath passcode) in findShortestPath' $ SearchZone start start
  where
    findShortestPath' s@(SearchZone _ c) = case find (\(Path _ _ _ p) -> p == goal) c of
        Nothing -> findShortestPath' $ expandSearchZone s
        Just p -> p

findLongestPath :: String -> Path
findLongestPath passcode = let start = (Set.singleton $ newPath passcode) in findLongestPath' $ SearchZone start start
  where
    findLongestPath' s@(SearchZone v c)
        | null c = longestPath v
        | otherwise = findLongestPath' $ expandSearchZone s
    longestPath = maximumBy (compare `on` pathLength) . Set.toList . Set.filter (\(Path _ _ _ p) -> p == goal)

-- Final, top-level exports
day17 :: String -> String
day17 = path . findShortestPath

day17' :: String -> Int
day17' = pathLength . findLongestPath

-- Input
run :: IO ()
run = do
    putStrLn "Day 17 results: "
    let input = "lpvhkcbi"
    putStrLn $ "  " ++ show (day17 input)
    putStrLn $ "  788 (cached)"
