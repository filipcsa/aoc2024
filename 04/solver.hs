import Text.Regex.TDFA ( (=~), AllMatches(getAllMatches), AllTextMatches (getAllTextMatches) )
import Prelude hiding (lookup)
import Data.List (transpose)

main :: IO ()
main = do
  contents <- getContents
  let inputLines = lines contents
  let ls = allDirections inputLines
  print $ sum $ map countXmasOccurences ls

allDirections :: [String] -> [String]
allDirections ls = ls ++ transpose ls ++ diagonalLR ls ++ diagonalRL ls

diagonalLR :: [String] -> [String]
diagonalLR strs = map (toDiagStr . extendDiagonal) diagonalStarts where
  n = length strs - 1
  diagonalStarts = [(0, c) | c <- [0..n]] ++ [(r, 0) | r <- [1..n]]
  extendDiagonal (r, c) = takeWhile (\(r', c') -> r' <= n && c' <= n) [(r + i, c + i) | i <- [0..]]
  toDiagStr coors = [ strs !! r !! c | (r, c) <- coors ]

diagonalRL :: [String] -> [String]
diagonalRL strs = map (toDiagStr . extendDiagonal) diagonalStarts where
  n = length strs - 1
  diagonalStarts = [(0,c) | c <- [0..n]] ++ [(r, n) | r <- [1..n]]
  extendDiagonal (r, c) = takeWhile (\(r', c') -> r' <= n && c' >= 0) [(r + i, c - i) | i <- [0..]]
  toDiagStr coors = [ strs !! r !! c | (r, c) <- coors ]

countXmasOccurences :: String -> Int
countXmasOccurences str = length dirMatches + length revMatches where
  dirMatches = getAllTextMatches (str =~ "XMAS") :: [String]
  revMatches = getAllTextMatches (reverse str =~ "XMAS") :: [String]