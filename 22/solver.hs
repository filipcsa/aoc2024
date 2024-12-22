import Data.Bits (Bits(xor))
import qualified Data.Map as M

main :: IO ()
main = do
  contents <- getContents
  let secretNums = map read $ lines contents :: [Int]
  let evolved = map (take 2001 . iterate evolve) secretNums
  let lastDigits = map (map (`mod` 10)) evolved
  let changes = map (\s -> zipWith (\ a b -> a - b) (tail s) s) lastDigits
  let zippedSeqs = map (\(cs, ds) -> zip cs (tail ds)) $ zip changes lastDigits
  let bananaSeqs = map (bananasPerSeq M.empty) zippedSeqs
  let sumBananasPerSeq = foldl collectBananas M.empty (concatMap M.toList bananaSeqs)
  print $ sum $ map last evolved
  print $ maximum $ M.elems sumBananasPerSeq

collectBananas :: M.Map (Int, Int, Int, Int) Int -> ((Int, Int, Int, Int), Int) -> M.Map (Int, Int, Int, Int) Int
collectBananas acc (sq, n) = M.insert sq n' acc where 
  n' = n + M.findWithDefault 0 sq acc

bananasPerSeq :: M.Map (Int, Int, Int, Int) Int -> [(Int, Int)] -> M.Map (Int, Int, Int, Int) Int
bananasPerSeq bananaSeq (a:b:c:d:es)
  | sq `M.member` bananaSeq = bananasPerSeq bananaSeq (b:c:d:es)
  | otherwise = bananasPerSeq bananaSeq' (b:c:d:es) where
    sq = (fst a, fst b, fst c, fst d)
    v = snd d
    bananaSeq' = M.insert sq v bananaSeq
bananasPerSeq bananaSeq _ = bananaSeq

evolve :: Int -> Int
evolve secret = result where
  s1 = xor secret (secret * 64) `mod` 16777216
  s2 = xor s1 (s1 `div` 32) `mod` 16777216
  result = xor s2 (s2 * 2048) `mod` 16777216
