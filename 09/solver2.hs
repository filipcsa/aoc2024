import Prelude hiding (lookup)
import qualified Data.Set as S

type Mem = (Int, Int, Int)

main :: IO ()
main = do
  contents <- getContents
  let inputLine = map (\c -> read [c]) $ head $ lines contents :: [Int]
  let expandedMemory = expandMemory inputLine 0 0
  let defragmented = foldl leftShiftBlock expandedMemory (reverse [1..length expandedMemory - 1])
  print $ checksum defragmented

checksum :: [(Int, Int, Int)] -> Int
checksum [] = 0
checksum ((id, l, u):rest) = sum [id*p | p <- [l..u]] + checksum rest

leftShiftBlock :: [(Int, Int, Int)] -> Int -> [(Int, Int, Int)]
leftShiftBlock mem blockId = (init $ leftShiftBlock' (take (blockPos + 1) mem) (mem !! blockPos)) ++ drop (blockPos + 1) mem where
  blockPos = length $ takeWhile (\(id, _, _) -> id /= blockId) mem

leftShiftBlock' :: [(Int, Int, Int)] -> (Int, Int, Int) -> [(Int, Int, Int)]
leftShiftBlock' [l] block = [l,block]
leftShiftBlock' ((fId, fl, fu):(sId, sl, su):rest) (bId, bl, bu)
  | (sl - fu) > (bu - bl + 1) = (fId, fl, fu) : (bId, fu+1, fu + 1 + (bu-bl)) : (sId, sl, su) : rest
  | otherwise = (fId, fl, fu) : leftShiftBlock' ((sId, sl, su):rest) (bId, bl, bu)

expandMemory :: [Int] -> Int -> Int -> [(Int, Int, Int)]
expandMemory [] _ _= []
expandMemory (b:bs) id pos
  | even id = (id `div` 2, pos, pos + b - 1) : expandMemory bs (id+1) (pos + b)
  | otherwise = expandMemory bs (id+1) (pos + b)