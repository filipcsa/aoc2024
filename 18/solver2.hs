import Text.Regex.TDFA ( (=~), AllMatches(getAllMatches), AllTextMatches (getAllTextMatches) )
import Prelude hiding (lookup)
import Data.List.Split (splitOn)
import qualified Data.Set as S
import qualified Data.PQueue.Prio.Min as Q
import qualified Data.Map as M

s :: Int
s = 70
type Pos = (Int, Int)

main :: IO ()
main = do
  contents <- getContents
  let inputLines = lines contents
  let allMems = map parsePos inputLines
  let q = Q.singleton 0 (s, s)
  let costs = M.singleton (s, s) 0
  let idx = head $ dropWhile (\i -> dijkstra (S.fromList $ take i allMems) costs q) [1024..]
  print $ allMems !! (idx - 1)

dijkstra :: S.Set Pos -> M.Map Pos Int -> Q.MinPQueue Int Pos -> Bool
dijkstra mems costs q
  | Q.size q == 0 = False
  | pos == (0, 0) = True
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
