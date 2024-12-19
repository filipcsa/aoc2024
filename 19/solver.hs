import Data.List.Split (splitOn)
import qualified Data.Set as S
import Data.List (isPrefixOf)

main :: IO ()
main = do
  contents <- getContents
  let inputLines = lines contents
  let (patterns, designs) = parseInput inputLines
  let possibles = filter (isPossible (S.singleton 0) patterns) designs
  print $ length possibles

isPossible :: S.Set Int -> [String] -> String -> Bool
isPossible poss patterns design
  | length design `S.member` poss = True
  | null poss = False
  | otherwise = isPossible poss' patterns design where
    minPos = minimum poss
    toMatch = drop minPos design
    newPoss = map ((+minPos) . length) (filter (`isPrefixOf` toMatch) patterns)
    poss' = foldl (flip S.insert) (S.delete minPos poss) newPoss

parseInput :: [String] -> ([String], [String])
parseInput strs = (patterns, designs) where
  [pats, designs] = splitOn [""] strs
  patterns = splitOn ", " $ head pats
  