import Text.Regex.TDFA ( (=~), AllMatches(getAllMatches), AllTextMatches (getAllTextMatches) )
import Prelude hiding (lookup)
import qualified Data.Set as S
import qualified Data.Map as M
import Data.List (nub, groupBy, sortBy, sort)
import Data.Ord (comparing)
import Data.Function (on)

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
  let ns = map numSides regions
  let prices = map (\(a, p) -> a * p) (zip areas ns)
  print ns
  print $ sum prices

numSides :: S.Set Pos -> Int
numSides region = l + r + u + d where
  poss = S.toList region
  lcs = filter (\(x, y) -> not ((x, y - 1) `S.member` region)) poss
  lls = groupByKey snd lcs
  l = sum $ map (countD fst) (M.elems lls)

  rcs = filter (\(x, y) -> not ((x, y + 1) `S.member` region)) poss
  rrs = groupByKey snd rcs
  r = sum $ map (countD fst) (M.elems rrs)

  ucs =  filter (\(x, y) -> not ((x-1, y )`S.member` region)) poss
  uus = groupByKey fst ucs
  u = sum $ map (countD snd) (M.elems uus)
  
  dcs = filter (\(x, y) -> not ((x+1, y )`S.member` region)) poss
  dds = groupByKey fst dcs
  d = sum $ map (countD snd) (M.elems dds)

countD :: ((Int, Int) -> Int) -> [(Int, Int)] -> Int
countD g as = 1 + tt where
  sortedMapped = sort $ map g as
  tt = length $ filter (\(a, b) -> a < b-1) (zip sortedMapped (tail sortedMapped))


groupByKey :: (Ord k) => (v -> k) -> [v] -> M.Map k [v]
groupByKey getkey
  = M.fromListWith (++) . fmap (\val -> (getkey val, [val]))

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
