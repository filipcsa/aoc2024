import qualified Data.Set as S
import qualified Data.Map as M

type Pos = (Int, Int)

allowedCheat :: Int
allowedCheat = 20

minSaving :: Int
minSaving = 100

main :: IO ()
main = do
  contents <- getContents
  let strs = lines contents
  let s = length strs - 1
  let start = head [(x,y) | x <- [0..s], y <- [0..s], strs !! x !! y == 'S']
  let originalPath = dos strs start []
  let posByDistFromStart = zip originalPath [0..]
  let cheatSaveMap = evalCheats posByDistFromStart M.empty
  print $ length originalPath
  print $ sum $ map snd $ filter (\(s, n) -> s >= minSaving) $ M.toList cheatSaveMap

-- key is the time saved, value is the actual number of such cheats
evalCheats :: [(Pos, Int)] ->  M.Map Int Int -> M.Map Int Int
evalCheats [] cheatMap = cheatMap
evalCheats (p:poss) cheatMap = evalCheats poss cheatMap' where
  cheatMap' = foldl (evalCheat p) cheatMap poss

evalCheat :: (Pos, Int) -> M.Map Int Int -> (Pos, Int) -> M.Map Int Int
evalCheat (fromPos, fromDist) cheatMap (toPos, toDist) = cheatMap' where
  cheatDist = dist fromPos toPos
  saved = toDist - fromDist - cheatDist
  cheatMap' = if cheatDist > allowedCheat && saved > 0
    then cheatMap
    else M.insert saved n cheatMap where
      n = 1 + M.findWithDefault 0 saved cheatMap

dist :: Pos -> Pos -> Int
dist (x1, y1) (x2, y2) = abs (x1-x2) + abs (y1-y2)

-- depth only search 
dos :: [String] -> Pos -> [Pos] -> [Pos]
dos strs (x,y) path
  | strs !! x !! y == 'E' = path'
  | otherwise = dos strs pos' path' where
    path' = path ++ [(x,y)]
    nextPoss = filter (\(x',y') -> strs !! x' !! y' /= '#')
      $ filter (\p -> null path || p /= last path) $ neighbors strs (x,y)
    pos' = head nextPoss

neighbors :: [String] -> Pos -> [Pos]
neighbors strs (x,y) = filter (\(x,y) -> x >= 0 && x < s && y >= 0 && y < s)
  [(x+1,y), (x-1,y), (x,y+1), (x,y-1)] where s = length strs