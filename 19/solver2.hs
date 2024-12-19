import Data.List.Split (splitOn)
import qualified Data.Map as M
import Data.List (isPrefixOf, minimumBy)
import Data.Function (on)

main :: IO ()
main = do
  contents <- getContents
  let inputLines = lines contents
  let (patterns, designs) = parseInput inputLines
  print $ sum $ map (isPossible (M.singleton 0 1) patterns) designs

isPossible :: M.Map Int Int -> [String] -> String -> Int
isPossible poss patterns design
  | null poss = 0
  | length design == minPos = M.findWithDefault 0 minPos poss
  | otherwise = isPossible poss' patterns design where
    (minPos, n) = minimumBy (compare `on` fst) $ M.toList poss
    toMatch = drop minPos design
    newPoss = map ((+minPos) . length) (filter (`isPrefixOf` toMatch) patterns)
    poss' = foldl (\acc p -> M.insert p (n+M.findWithDefault 0 p poss) acc) (M.delete minPos poss) newPoss

parseInput :: [String] -> ([String], [String])
parseInput strs = (patterns, designs) where
  [pats, designs] = splitOn [""] strs
  patterns = splitOn ", " $ head pats
