import Text.Regex.TDFA ( (=~), AllMatches(getAllMatches), AllTextMatches (getAllTextMatches) )
import Prelude hiding (lookup)
import Data.List (transpose)

main :: IO ()
main = do
  contents <- getContents
  let inputLines = lines contents
  print $ countCrossMases inputLines

countCrossMases :: [String] -> Int
countCrossMases strs = length $ filter formsCrossMas aposs where
  n = length strs - 1
  aposs = [ (r,c) | r <- [1..(n-1)], c <- [1..(n-1)], strs !! r !! c == 'A']
  formsCrossMas (r,c) = isMas (map pos2Char [(r-1, c-1), (r,c), (r+1, c+1)]) && isMas (map pos2Char [(r-1, c+1), (r,c), (r+1, c-1)])
  pos2Char (r,c) = strs !! r !! c
  isMas str = str == "MAS" || str == "SAM"

