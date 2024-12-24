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
  validateGates gates $ M.toList gates

validateGates :: Gates -> [(String, (String, String, String))] -> IO ()
validateGates _ [] = return ()
validateGates gates ((result, (a, op, b)):gts) = do
  case op of
    "XOR" -> validateXor gates (result, (a, op, b))
    "AND" -> validateAnd gates (result, (a, op, b))
    "OR" -> validateOr gates (result, (a, op, b))
  validateGates gates gts

-- or is preceded by 2 end gates
validateOr :: Gates -> (String, (String, String, String)) -> IO ()
validateOr _ ("z45", _) = return ()
validateOr gates (res, _) = if S.fromList succOps == S.fromList ["AND", "XOR"]
  then return ()
  else print res 
  where succOps = map (\(_, op, _) -> op) $ filter (\(a, _, b) -> a == res || b == res) $ M.elems gates

-- and is succeeded by or
validateAnd :: Gates -> (String, (String, String, String)) -> IO ()
validateAnd _ (_, ("y00", _, "x00")) = return () -- case of half adder
validateAnd gates (res, _) = if succOps == ["OR"]
  then return ()
  else print res 
  where succOps = map (\(_, op, _) -> op) $ filter (\(a, _, b) -> a == res || b == res) $ M.elems gates

-- xor succeeded by output / preceded by input
validateXor :: Gates -> (String, (String, String, String)) -> IO ()
validateXor _ ('z':zs, (_, _, _)) = return ()
validateXor gates (res, (p1:p1s, _, p2:p2s)) = if 
  S.fromList succOps == S.fromList ["AND", "XOR"]
  && ((p1, p2) == ('x', 'y') || (p1,p2) == ('y','x'))
  then return ()
  else print res 
  where succOps = map (\(_, op, _) -> op) $ filter (\(a, _, b) -> a == res || b == res) $ M.elems gates

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
