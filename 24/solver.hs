import Text.Regex.TDFA ( (=~), AllMatches(getAllMatches), AllTextMatches (getAllTextMatches) )
import qualified Data.Map as M
import qualified Data.Set as S
import Data.List.Split (splitOn)
import Data.Bits (Bits(xor))
import Data.List (sortBy)
import Data.Function (on)

type Gates = M.Map String (String, String, String)

main :: IO ()
main = do
  contents <- getContents
  let inputLines = lines contents
  let (initVals, gates) = parseInput inputLines
  let sortedNodes = toposort (S.fromList $ M.keys initVals) (M.toList gates) (M.keys initVals)
  let finalVals = foldl (progressGate gates) initVals sortedNodes
  let zBin = reverse $ map snd $ sortBy (compare `on` fst) $ filter (\(n, v) -> head n == 'z') $ M.toList finalVals
  print $ bin2num zBin

bin2num :: [Int] -> Int
bin2num = foldl (\acc b -> 2*acc + b) 0

progressGate :: Gates -> M.Map String Int -> String -> M.Map String Int
progressGate gates vals node
  | node `M.member` vals = vals
  | otherwise = M.insert node val vals where 
    (a, op, b) = M.findWithDefault ("", "", "") node gates
    aVal = M.findWithDefault 0 a vals
    bVal = M.findWithDefault 0 b vals
    val = case op of
      "AND" -> min aVal bVal
      "OR" -> max aVal bVal
      "XOR" -> xor aVal bVal

toposort :: S.Set String -> [(String, (String, String, String))] -> [String] -> [String]
toposort _ [] sorted = sorted
toposort processed gates sorted = toposort processed' gates' (sorted ++ new) where
  new = map fst $ filter (\(n, (a,_,b)) -> a `S.member` processed && b `S.member` processed) gates
  gates' = filter (\(n, (a,_,b)) -> a `S.notMember` processed || b `S.notMember` processed) gates
  processed' = foldl (flip S.insert) processed new

parseInput :: [String] -> (M.Map String Int, Gates)
parseInput strs = (initVals, gates) where
  [valStrs, gateStrs] = splitOn [""] strs
  initVals = foldl parseVal M.empty valStrs
  gates = foldl parseGate M.empty gateStrs

parseGate :: Gates -> String -> Gates
parseGate gates str = M.insert c (a, op, b) gates where
  (_,_,_,[a,op,b,c]) = str =~ "([a-zA-Z0-9]+) (AND|OR|XOR) ([a-zA-Z0-9]+) -> ([a-zA-Z0-9]+)" :: (String, String, String, [String])

parseVal :: M.Map String Int -> String -> M.Map String Int
parseVal acc str = M.insert v (read b) acc where
  [v, b] = splitOn ": " str
