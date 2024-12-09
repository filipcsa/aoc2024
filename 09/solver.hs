import Prelude hiding (lookup)

main :: IO ()
main = do
  contents <- getContents
  let inputLine = map (\c -> read [c]) $ head $ lines contents :: [Int]
  let expandedMemory = expandMemory inputLine 0
  print $ length expandedMemory
  let defragmentedMemory = defragmentMemory2 expandedMemory
  print $ checksum defragmentedMemory 0

checksum :: [Int] -> Int -> Int
checksum [] _ = 0
checksum (n:ns) pos = n*pos + checksum ns (pos+1)

defragmentMemory2 :: [Int] -> [Int]
defragmentMemory2 [] = []
defragmentMemory2 ms
  | last ms == (-1) = defragmentMemory2 (init ms)
  | head ms /= (-1) = head ms : defragmentMemory2 (tail ms)
  | otherwise = last ms : defragmentMemory2 (init $ tail ms)

defragmentMemory :: [Int] -> Int -> Int -> [Int]
defragmentMemory mem s f
  | f == s = take (s + 1) mem
  | mem !! f == (-1) = defragmentMemory mem s (f-1)
  | mem !! s /= (-1) = defragmentMemory mem (s+1) f
  | otherwise = defragmentMemory mem' (s+1) (f-1) where
    memAtF = mem !! f
    mem' = take s mem ++ [memAtF] ++ drop (s+1) mem

expandMemory :: [Int] -> Int -> [Int]
expandMemory [] _ = []
expandMemory (n:ns) blockNum = replicate n memId ++ expandMemory ns (blockNum + 1) where
  memId = if even blockNum
    then blockNum `div` 2
    else (-1)