import qualified Data.Map as M
import qualified Data.Set as S
import Data.List.Split (splitOn)

main :: IO ()
main = do
  contents <- getContents
  let inputLines = lines contents
  let adjacencies = foldl parseEdge M.empty inputLines
  let triplets = foldl (findCyclesFrom adjacencies) S.empty $ M.keys adjacencies
  let ts = filter (\trip -> any (\s -> head s == 't') $ S.toList trip) $ S.toList triplets
  print $ length ts

findCyclesFrom :: M.Map String (S.Set String) -> S.Set (S.Set String) -> String -> S.Set (S.Set String)
findCyclesFrom adjs components node = foldl (\acc c -> S.insert c acc) components newCycles where
  succs = S.toList $ M.findWithDefault S.empty node adjs
  newCycles = map (\(n,a,b) -> S.fromList [n,a,b]) 
    $ filter (\(_, a, b) -> b `S.member` M.findWithDefault S.empty a adjs) [(node, a, b) | a <- succs, b <- succs, a /= b]

parseEdge :: M.Map String (S.Set String) -> String -> M.Map String (S.Set String)
parseEdge adjs str = adjs' where
  [n1, n2] = splitOn "-" str
  n1Neighbors = S.insert n2 $ M.findWithDefault S.empty n1 adjs
  n2Neighbors = S.insert n1 $ M.findWithDefault S.empty n2 adjs
  adjs' = M.insert n1 n1Neighbors $ M.insert n2 n2Neighbors adjs
