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
  -- print games
  print $ sum gameCosts

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
  [px, py] = map read pm
