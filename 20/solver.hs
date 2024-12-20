import qualified Data.Set as S
import qualified Data.Map as M
import Data.List (minimumBy, maximumBy)
import Data.Function (on)

type Pos = (Int, Int)

main :: IO ()
main = do
  contents <- readFile "input.txt"
  let strs = lines contents
  let s = length strs - 1
  let start = head [(x,y) | x <- [0..s], y <- [0..s], strs !! x !! y == 'S']
  let originalPath = dos strs start (-1,-1) 0 M.empty M.empty
  let cheatWalls = findCheatWalls strs (M.keys originalPath) S.empty
  print $ length cheatWalls
  let cheatSaveMap = foldl (evalCheat strs start originalPath) M.empty cheatWalls
  let actualSaves = map (\(s,n) -> ((length originalPath) - s, n)) $ M.toList cheatSaveMap
  print $ length originalPath
  print $ sum $ map snd $ filter (\(s, n) -> s >= 100) actualSaves

evalCheat :: [String] -> Pos -> M.Map Pos Int -> M.Map Int Int -> Pos -> M.Map Int Int
evalCheat strs start origPath saves cheatWall
  | M.size cheatPath > 0 = M.insert improvement (1 + M.findWithDefault 0 improvement saves) saves
  | otherwise = saves where
    cheatPath = dos strs start cheatWall 0 origPath M.empty
    improvement = length cheatPath

findCheatWalls :: [String] -> [Pos] -> S.Set Pos -> S.Set Pos
findCheatWalls strs [] cheatWalls = cheatWalls
findCheatWalls strs (pos:poss) cheatWalls = findCheatWalls strs poss cheatWalls' where
  neighborWalls = filter (\(x,y) -> strs !! x !! y == '#') $ neighbors strs pos
  cheatWalls' = foldl (flip S.insert) cheatWalls neighborWalls

-- depth only search 
dos :: [String] -> Pos -> Pos -> Int -> M.Map Pos Int -> M.Map Pos Int -> M.Map Pos Int
dos strs (x,y) cheatWall n origPath visited
  | strs !! x !! y == 'E' = M.insert (x,y) n visited
  | null nextPoss = M.empty
  | otherwise = dos strs pos' cheatWall (n+1) origPath visited' where
    visited' = M.insert (x,y) n visited
    nextPoss = filter (\(x',y') -> (strs !! x' !! y' /= '#') || (x',y') == cheatWall)
      $ filter (`M.notMember` visited') $ neighbors strs (x,y)
    pos' = if length nextPoss > 1 then
      if cheatWall `elem` nextPoss
        then cheatWall
        else fst $ maximumBy (compare `on` snd) $ map (\p -> (p, M.findWithDefault 0 p origPath)) nextPoss
      else head nextPoss

neighbors :: [String] -> Pos -> [Pos]
neighbors strs (x,y) = filter (\(x,y) -> x >= 0 && x < s && y >= 0 && y < s)
  [(x+1,y), (x-1,y), (x,y+1), (x,y-1)] where s = length strs