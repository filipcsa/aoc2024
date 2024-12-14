import Text.Regex.TDFA ( (=~), AllMatches(getAllMatches), AllTextMatches (getAllTextMatches) )
import Prelude hiding (lookup)
import qualified Data.Set as S

type Pos = (Int, Int)
type Robot = (Pos, Pos)

h :: Int
h = 103
w :: Int
w = 101
iterations :: Int
iterations = 100

main :: IO ()
main = do
  contents <- getContents
  let inputLines = lines contents
  let initRobots = map parseRobot inputLines
  let finalRobots = map progressRobot initRobots
  print $ safetyFactor finalRobots

safetyFactor :: [Robot] -> Int
safetyFactor robots = tl * tr * bl * br where
  poss = map fst robots
  tl = length $ filter (\(x, y) -> x < h `div` 2 && y < w `div` 2) poss
  tr = length $ filter (\(x, y) -> x < h `div` 2 && y > w `div` 2) poss
  bl = length $ filter (\(x, y) -> x > h `div` 2 && y < w `div` 2) poss
  br = length $ filter (\(x, y) -> x > h `div` 2 && y > w `div` 2) poss

progressRobot :: Robot -> Robot
progressRobot ((x, y), (vx, vy)) = ((x', y'), (vx, vy)) where
  x' = (x + iterations * vx) `mod` h
  y' = (y + iterations * vy) `mod` w

parseRobot :: String -> Robot
parseRobot str = ((x, y), (vx, vy)) where
  regex = "p=(-*[0-9]+),(-*[0-9]+) v=(-*[0-9]+),(-*[0-9]+)"
  (_, _, _, ms) = str =~ regex :: (String, String, String, [String])
  [y, x, vy, vx] = map read ms
