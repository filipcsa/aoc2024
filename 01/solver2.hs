import Text.Regex.TDFA ( (=~), AllMatches(getAllMatches), AllTextMatches (getAllTextMatches) )
import Prelude hiding (lookup)
import Data.List (sort)
import Data.List.Split (splitOn)

main :: IO ()
main = do
  contents <- getContents
  let inputLines = lines contents
  let numPairs = map parsePair inputLines
  let (l1, l2) = (sort $ map fst numPairs, sort $ map snd numPairs)
  let simScore = map (computeSimScore l2) l1
  print $ sum simScore

computeSimScore :: [Int] -> Int -> Int
computeSimScore l x = x * length (filter (==x) l)

parsePair :: String -> (Int, Int)
parsePair str = (read a, read b) where
  [a, b] = splitOn "   " str