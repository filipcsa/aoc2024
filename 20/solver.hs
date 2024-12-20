import qualified Data.Set as S
import qualified Data.Map as M

type Pos = (Int, Int)

main :: IO ()
main = do
  -- contents <- getContents
  -- let strs = lines contents
  contents <- readFile "simple-input.txt"
  let strs = lines contents
  let s = length strs - 1
  let start = head [(x,y) | x <- [0..s], y <- [0..s], strs !! x !! y == 'S']
  let originalPath = dos strs start (-1,-1) S.empty
  let cheatWalls = findCheatWalls strs (S.toList originalPath) S.empty
  let cheatSaveMap = foldl (evalCheat strs start) M.empty cheatWalls
  print $ length originalPath
  print cheatSaveMap

evalCheat :: [String] -> Pos -> M.Map Int Int -> Pos -> M.Map Int Int
evalCheat strs start saves cheatWall
  | S.size cheatPath > 0 = M.insert improvement (1 + M.findWithDefault 0 improvement saves) saves
  | otherwise = saves where
    cheatPath = dos strs start cheatWall S.empty
    improvement = length cheatPath

findCheatWalls :: [String] -> [Pos] -> S.Set Pos -> S.Set Pos
findCheatWalls strs [] cheatWalls = cheatWalls
findCheatWalls strs (pos:poss) cheatWalls = findCheatWalls strs poss cheatWalls' where
  neighborWalls = filter (\(x,y) -> strs !! x !! y == '#') $ neighbors strs pos
  cheatWalls' = foldl (flip S.insert) cheatWalls neighborWalls

-- depth only search 
dos :: [String] -> Pos -> Pos -> S.Set Pos -> S.Set Pos
dos strs (x,y) (wx,wy) visited
  | strs !! x !! y == 'E' = S.insert (x,y) visited
  | null nextPoss = S.empty
  | otherwise = dos strs pos' (wx,wy) visited' where
    visited' = S.insert (x,y) visited
    nextPoss = filter (\(x',y') -> (strs !! x' !! y' /= '#') || (x'==wx && y==wy))
      $ filter (`S.notMember` visited') $ neighbors strs (x,y)
    pos' = if length nextPoss > 1 then (wx,wy) else head nextPoss

neighbors :: [String] -> Pos -> [Pos]
neighbors strs (x,y) = filter (\(x,y) -> x >= 0 && x < s && y >= 0 && y < s)
  [(x+1,y), (x-1,y), (x,y+1), (x,y-1)] where s = length strs