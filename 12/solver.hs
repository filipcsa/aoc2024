import Text.Regex.TDFA ( (=~), AllMatches(getAllMatches), AllTextMatches (getAllTextMatches) )
import Prelude hiding (lookup)
import qualified Data.Set as S

type Pos = (Int, Int)

main :: IO ()
main = do
  contents <- getContents
  let inputLines = lines contents
  let s = length inputLines - 1
  let allPoss = S.fromList [(x,y) | x <- [0..s], y <- [0..s]]
  let regions = findRegions inputLines allPoss
  let areas = map S.size regions
  let peris = map perimeter regions
  let prices = map (\(a, p) -> a * p) (zip areas peris)
  -- print regions
  -- print areas
  -- print peris
  print $ sum prices

perimeter :: S.Set Pos -> Int
perimeter region = sum $ map (\p -> 4 - numNeighborsInRegion region p) $ S.toList region where
  numNeighborsInRegion region pos = sum $ map (\n -> if n `S.member` region then 1 else 0) $ neighbors pos

findRegions :: [String] -> S.Set Pos -> [S.Set Pos]
findRegions strs unknown
  | null unknown = []
  | otherwise = newRegion : findRegions strs unknown' where
    (x,y) = head $ S.toList unknown
    crop = strs !! x !! y
    newRegion = dfsRegion strs crop (x,y) S.empty
    unknown' = S.difference unknown newRegion

dfsRegion :: [String] -> Char -> Pos -> S.Set Pos -> S.Set Pos
dfsRegion strs crop (x,y) region
  | (x,y) `S.member` region = region
  | x < 0 || x >= length strs || y < 0 || y >= length strs = region
  | strs !! x !! y /= crop = region
  | otherwise = foldl (\acc pos' -> S.union acc (dfsRegion strs crop pos' acc)) region' (neighbors (x,y)) where
    region' = S.insert (x,y) region

neighbors :: Pos -> [Pos]
neighbors (x,y) = [(x+1, y), (x-1, y), (x, y+1), (x, y-1)]
