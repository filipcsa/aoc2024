import Text.Regex.TDFA ( (=~), AllMatches(getAllMatches), AllTextMatches (getAllTextMatches) )
import Prelude hiding (lookup)
import qualified Data.Set as S
import Control.Concurrent (threadDelay)
import qualified Control.Monad

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
  run initRobots 1

-- observing that col 35 seems to form a continuous column
run :: [Robot] -> Int -> IO ()
run robots it = do
  let robots' = map iterateRobot robots
  let poss = map fst robots'
  let num35s = length $ filter (\(_,y) -> y == 35) poss
  Control.Monad.when (num35s > 30) $ do
    print $ "Iteration: " ++ show it
    printWorld robots'
    print ""
    threadDelay 100000
  run robots' (it+1)

iterateRobot :: Robot -> Robot
iterateRobot ((x, y), (vx, vy)) = ((x', y'), (vx, vy)) where
  x' = (x + vx) `mod` h
  y' = (y + vy) `mod` w

parseRobot :: String -> Robot
parseRobot str = ((x, y), (vx, vy)) where
  regex = "p=(-*[0-9]+),(-*[0-9]+) v=(-*[0-9]+),(-*[0-9]+)"
  (_, _, _, ms) = str =~ regex :: (String, String, String, [String])
  [y, x, vy, vx] = map read ms

printWorld :: [Robot] -> IO ()
printWorld robots =
  sequence_ [print [if (x,y) `S.member` poss then '#' else ' ' | y <- [0..w-1]] | x <- [0..h-1]]
  where poss = S.fromList $ map fst robots
