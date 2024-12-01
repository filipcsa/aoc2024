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
  let dists = zipWith (\ a b -> abs (a - b)) l1 l2
  print $ sum dists

parsePair :: String -> (Int, Int)
parsePair str = (read a, read b) where
  [a, b] = splitOn "   " str