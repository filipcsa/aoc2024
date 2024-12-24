import qualified Data.Map as M
import qualified Data.Set as S
import Data.List.Split (splitOn)
import Data.List (nub, maximumBy, sort, intercalate)
import Data.Function (on)

main :: IO ()
main = do
  contents <- getContents
  let inputLines = lines contents
  let adjacencies = foldl parseEdge M.empty inputLines
  let edges = map (splitOn "-") inputLines
  let allConns = foldl (processEdge adjacencies) [] edges
  let maxCon = sort $ maximumBy (compare `on` length) allConns
  print $ intercalate "," maxCon

processEdge :: M.Map String (S.Set String) -> [[String]] -> [String] -> [[String]]
processEdge adjs [] edge = [edge]
processEdge adjs (conn:conns) [a,b]
  | all (\n -> abSet `S.isSubsetOf` (S.insert n $ M.findWithDefault S.empty n adjs)) conn = nub (conn ++ [a,b]) : conns
  | otherwise = conn : processEdge adjs conns [a,b] where
    abSet = S.fromList [a,b]

parseEdge :: M.Map String (S.Set String) -> String -> M.Map String (S.Set String)
parseEdge adjs str = adjs' where
  [n1, n2] = splitOn "-" str
  n1Neighbors = S.insert n2 $ M.findWithDefault S.empty n1 adjs
  n2Neighbors = S.insert n1 $ M.findWithDefault S.empty n2 adjs
  adjs' = M.insert n1 n1Neighbors $ M.insert n2 n2Neighbors adjs
