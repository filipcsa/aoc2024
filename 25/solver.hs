import Data.List.Split (splitOn)

main :: IO ()
main = do
  contents <- getContents
  let inputLines = splitOn [""] $ lines contents
  let (ls', ks') = foldl (\(ls, ks) strs -> if head (head strs) == '#' then (strs:ls,ks) else (ls,strs:ks)) ([],[]) inputLines
  let (ls, ks) = (map (schema2Nums True) ls', map (schema2Nums False) ks')
  print $ length ["XMAS" | l <- ls, k <- ks, compatible l k]

compatible :: [Int] -> [Int] -> Bool
compatible ls ks = all (<=5) $ zipWith (+) ls ks

schema2Nums :: Bool -> [[Char]] -> [Int]
schema2Nums isLock strs = [ length (takeWhile (=='#') [strs !! x !! y | x <- xs]) - 1 | y <- [0..w]]
  where
    w = length (head strs) - 1
    h = length strs - 1
    xs = if isLock then [0..h] else reverse [0..h]
    