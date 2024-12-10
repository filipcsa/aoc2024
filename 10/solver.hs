import Prelude hiding (lookup)
import Data.List (nub)

type Pos = (Int, Int)

main :: IO ()
main = do
  contents <- getContents
  let topoMap = map (map (\c -> read [c])) $ lines contents :: [[Int]]
  let poss0 = [(r, c) | r <- [0..length topoMap - 1], c <- [0..length topoMap - 1], topoMap !! r !! c == 0]
  print poss0
  let score = sum $ map length $ map (trailhead topoMap) poss0 -- add nub (unique map) to solve 1
  print score

trailhead :: [[Int]] -> (Int, Int) -> [Pos]
trailhead topoMap (r, c)
  | curr == 9 = [(r,c)]
  | length succs > 0 = concat [trailhead topoMap succ | succ <- succs]
  | otherwise = []
  where
    curr = topoMap !! r !! c
    s = length topoMap 
    neighbors = filter (\(r',c') -> r' >= 0 && r' < s && c' >= 0 && c' < s) [(r+1, c), (r-1, c), (r, c+1), (r, c-1)]
    succs = [(r', c') | (r', c') <- neighbors, topoMap !! r' !! c' - 1 == curr]
