import Prelude hiding (lookup)
import Data.List.Split (splitOn)
import qualified Data.Map as M
import qualified Data.Set as S
import Data.List (sortBy)

type Succ = M.Map Int (S.Set Int)

main :: IO ()
main = do
  contents <- getContents
  let inputLines = lines contents
  let [precs, updateStrs] = splitOn [""] inputLines
  let succ = foldl addPrec M.empty precs
  let updates = map (map read . splitOn ",") updateStrs :: [[Int]]
  let incorrectUpdates = filter (not . correctlyOrdered succ) updates
  let fixedUpdates = map (fixUpdate succ) incorrectUpdates
  print $ sum $ map (\u -> u !! (length u `div` 2)) fixedUpdates

fixUpdate :: Succ -> [Int] -> [Int]
fixUpdate succ = sortBy (succComparator succ)

succComparator :: Succ -> Int -> Int -> Ordering
succComparator succ a b = if S.member b (M.findWithDefault S.empty a succ)
  then LT
  else GT

correctlyOrdered :: Succ -> [Int] -> Bool
correctlyOrdered _ [n] = True
correctlyOrdered succMap (n:ns) = S.isSubsetOf (S.fromList ns) (M.findWithDefault S.empty n succMap) && correctlyOrdered succMap ns

addPrec :: Succ -> String -> Succ
addPrec succMap str = M.insert pred succs succMap where
  [pred, succ] = map read $ splitOn "|" str
  succs = S.insert succ $ M.findWithDefault S.empty pred succMap
