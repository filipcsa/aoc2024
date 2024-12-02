import Text.Regex.TDFA ( (=~), AllMatches(getAllMatches), AllTextMatches (getAllTextMatches) )
import Prelude hiding (lookup)
import Data.List.Split (splitOn)

main :: IO ()
main = do
  contents <- getContents
  let inputLines = lines contents
  let reports = map parseReport inputLines
  let numSafe = length $ filter isSafe reports
  print numSafe

parseReport :: String -> [Int]
parseReport str = map read $ splitOn " " str

isSafe :: [Int] -> Bool
isSafe report = (allSubs (uncurry (<)) report || allSubs (uncurry (>)) report) && allSubs (\(a, b) -> abs (a-b) >= 1 && abs (a-b) <= 3) report

allSubs :: ((Int, Int) -> Bool) -> [Int] -> Bool
allSubs cond report = all cond $ zip report (tail report)