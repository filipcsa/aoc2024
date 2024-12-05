import Prelude hiding (lookup)
import Data.List.Split (splitOn)
import qualified Data.Map as M
import qualified Data.Set as S

type Succ = M.Map Int (S.Set Int)

main :: IO ()
main = do
  contents <- getContents
  let inputLines = lines contents
  let [precs, updateStrs] = splitOn [""] inputLines
  let succ = foldl addPrec M.empty precs
  let updates = map (map read . splitOn ",") updateStrs :: [[Int]]
  let correctUpdates = filter (correctlyOrdered succ) updates
  print $ sum $ map (\u -> u !! (length u `div` 2)) correctUpdates

correctlyOrdered :: Succ -> [Int] -> Bool
correctlyOrdered _ [n] = True
correctlyOrdered succMap (n:ns) = S.isSubsetOf (S.fromList ns) (M.findWithDefault S.empty n succMap) && correctlyOrdered succMap ns

addPrec :: Succ -> String -> Succ
addPrec succMap str = M.insert pred succs succMap where
  [pred, succ] = map read $ splitOn "|" str
  succs = S.insert succ $ M.findWithDefault S.empty pred succMap
