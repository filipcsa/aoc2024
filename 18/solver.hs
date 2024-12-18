import Text.Regex.TDFA ( (=~), AllMatches(getAllMatches), AllTextMatches (getAllTextMatches) )
import Prelude hiding (lookup)
import Data.List.Split (splitOn)
import qualified Data.Set as S
import qualified Data.PQueue.Prio.Min as Q
import qualified Data.Map as M

s :: Int
s = 70
type Pos = (Int, Int)

fallen :: Int
fallen = 1024

main :: IO ()
main = do
  contents <- getContents
  let inputLines = lines contents
  let mems = S.fromList $ take fallen $ map parsePos inputLines
  let q = Q.singleton 0 (s, s)
  let costs = M.singleton (s, s) 0
  d <- dijkstra mems costs q
  print d

dijkstra :: S.Set Pos -> M.Map Pos Int -> Q.MinPQueue Int Pos -> IO Int
dijkstra mems costs q
  | pos == (0, 0) = return cost
  | otherwise = do
    -- print succs
    dijkstra mems costs' q' where
    (cost, pos) = Q.findMin q
    q0 = Q.deleteMin q
    succs = filter (\s -> (cost+1) < M.findWithDefault 100000 s costs) $ findSuccs mems pos :: [Pos]
    costs' = foldl (\acc p -> M.insert p (cost+1) acc) costs succs
    q' = foldl (flip (Q.insert (cost+1))) q0 succs

findSuccs :: S.Set Pos -> Pos -> [Pos]
findSuccs mems (x,y) = filter (`S.notMember` mems)
  $ filter (\(x',y') -> x' >= 0 && x' <= s && y' >= 0 && y' <= s)
  [(x+1, y), (x-1,y), (x, y+1), (x, y-1)]

parsePos :: String -> Pos
parsePos str = (read x, read y) where
  [y, x] = splitOn "," str
