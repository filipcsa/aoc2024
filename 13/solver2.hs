import Text.Regex.TDFA ( (=~), AllMatches(getAllMatches), AllTextMatches (getAllTextMatches) )
import Prelude hiding (lookup)
import Data.List.Split (splitOn)
import Data.List (sortBy, sort)

type Game = [(Int, Int)]

main :: IO ()
main = do
  contents <- getContents
  let inputLines = lines contents
  let games = map parseGame $ splitOn [""] inputLines
  let gameCosts = map playGame games
  let solutions = map solve games
  let correctSolution = map snd $ filter isCorrect (zip games solutions)
  print $ sum $ map priceOfSolution correctSolution

priceOfSolution :: (Int, Int) -> Int
priceOfSolution (a, b) = 3*a + b

isCorrect :: (Game, (Int, Int)) -> Bool
isCorrect ([(a1, a2), (b1, b2), (p1, p2)], (x, y)) = f && s where
  f = a1*x + b1*y == p1
  s = a2*x + b2*y == p2
  -- print $ sum gameCosts

-- Linear equation, manual solution to avoid matrices ... 
-- a1x + b1y = p1
-- a2x + b2y = p2
-- find lcm of b1 and b2, the find the value of x and substitue to find y 
solve :: Game -> (Int, Int)
solve [(a1, a2), (b1, b2), (p1, p2)] = (x, y) where 
  b1b2lcm = lcm b1 b2
  eq1Mult = b1b2lcm `div` b1
  eq2Mult = b1b2lcm `div` b2
  aDiff = (a1 * eq1Mult) - (a2 * eq2Mult)
  pDiff = (p1* eq1Mult) - (p2 * eq2Mult)
  x = pDiff `div` aDiff
  y = (p1 - a1*x) `div` b1 



playGame :: Game -> Int
playGame [(ax, ay), (bx, by), (px, py)] = cost where
  possibleWinMoves = [(as, bs) | as <- [0..100], bs <- [0..100], as * ax + bs * bx == px && as * ay + bs * by == py]
  moveCosts = map (\(a, b) -> 3*a + b) possibleWinMoves

  minWinMove = sort moveCosts--(\(a1, b1) (a2, b2) -> compare (3*a1 + b1) (3*a2 + b2)) possibleWinMoves
  cost = if null minWinMove then 0 else head minWinMove

parseGame :: [String] -> Game
parseGame [aStr, bStr, pStr] = [(ax, ay), (bx, by), (px, py)] where
  regexA = "Button A: X\\+([0-9]+), Y\\+([0-9]+)"
  regexB = "Button B: X\\+([0-9]+), Y\\+([0-9]+)"
  regexP = "Prize: X=([0-9]+), Y=([0-9]+)"
  (_, _, _, am) = aStr =~ regexA :: (String, String, String, [String])
  (_, _, _, bm) = bStr =~ regexB :: (String, String, String, [String])
  (_, _, _, pm) = pStr =~ regexP :: (String, String, String, [String])
  [ax, ay] = map read am
  [bx, by] = map read bm
  [px', py'] = map read pm
  [px, py] = [px' + 10000000000000, py' + 10000000000000]
