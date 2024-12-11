import Prelude hiding (lookup)
import Data.List (nub)
import Data.List.Split (splitOn)
import qualified Data.Map as M
import System.Posix.Files (accessModes)

main :: IO ()
main = do
  contents <- getContents
  let inputLine = head $ lines contents
  let initNums = map read $ splitOn " " inputLine :: [Int]
  let initOccs = foldl (\acc n -> M.insert n 1 acc) M.empty initNums :: M.Map Int Int
  print initOccs
  let finalOccs = run' initOccs 75
  -- print finalOccs
  print $ sum $ M.elems finalOccs

run' :: M.Map Int Int -> Int -> M.Map Int Int
run' occs 0 = occs
run' occs n = run' (progressOcc occs) (n-1)

progressOcc :: M.Map Int Int -> M.Map Int Int
progressOcc occs = foldl processEntry M.empty $ M.toList occs

processEntry :: M.Map Int Int -> (Int, Int) -> M.Map Int Int
processEntry acc (n, v) = 
  foldl (\a n' -> M.insert n' ((M.findWithDefault 0 n' a) + v) a) acc ns' where
    ns' = processNum n

run :: [Int] -> Int -> [Int]
run nums 0 = nums
run nums n = run (concatMap processNum nums) (n - 1) 

processNum :: Int -> [Int]
processNum 0 = [1]
processNum n
  | even (length digits) = splitTwo digits
  | otherwise = [n * 2024] where
    digits = show n

splitTwo :: String -> [Int]
splitTwo str = [read f, read s] where
  f = take h str
  s = drop h str
  h = length str `div` 2
