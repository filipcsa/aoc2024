import Text.Regex.TDFA ( (=~), AllMatches(getAllMatches), AllTextMatches (getAllTextMatches) )
import Prelude hiding (lookup)
import Data.List.Split (splitOn)

type Equation = (Int, [Int])

main :: IO ()
main = do
  contents <- getContents
  let inputLines = lines contents
  let equations = map parseEq inputLines
  let validEquations = filter isValidEq equations
  print $ sum $ map fst validEquations

isValidEq :: Equation -> Bool
isValidEq (res, n:ns) = elem res $ generateResults n ns

generateResults :: Int -> [Int] -> [Int]
generateResults acc [] = [acc]
generateResults acc (n:ns) = 
  generateResults (acc + n) ns ++ 
  generateResults (acc * n) ns ++ 
  generateResults (read (show acc ++ show n)) ns

parseEq :: String -> Equation
parseEq str = (res, nums) where
  [resStr, numsStr] = splitOn ": " str
  res = read resStr
  nums = map read $ splitOn " " numsStr
