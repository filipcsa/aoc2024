import Prelude hiding (lookup)
import qualified Data.Set as S
import Data.List (nub)

type Pos = (Int, Int)
data Ori = U | D | L | R deriving Show

main :: IO ()
main = do
  contents <- getContents
  let inputLines = lines contents
  let mapSize = length inputLines
  let (blocks, guard) = initializeWorld inputLines
  let guardRoute = takeWhile (not . guardOutsideMap mapSize) $ iterate (simulateGuard mapSize blocks) guard
  print $ length $ nub $ map fst guardRoute

simulateGuard :: Int -> S.Set Pos -> (Pos, Ori) -> (Pos, Ori)
simulateGuard s blocks (pos, ori) = (pos', ori') where
  next = nextPos pos ori
  nextPosBlocked = S.member next blocks
  (pos', ori') = if nextPosBlocked then (pos, updateOri ori) else (next, ori)

updateOri :: Ori -> Ori
updateOri U = R 
updateOri R = D 
updateOri D = L 
updateOri L = U 

nextPos :: Pos -> Ori -> Pos
nextPos (r, c) U = (r-1, c)
nextPos (r, c) D = (r+1, c)
nextPos (r, c) L = (r, c-1)
nextPos (r, c) R = (r, c+1)


guardOutsideMap :: Int -> (Pos, Ori) -> Bool
guardOutsideMap s ((r, c), _) = r < 0 || r > (s - 1) || c < 0 || c > (s - 1)


initializeWorld :: [String] -> (S.Set Pos, (Pos, Ori))
initializeWorld strs = (blocks, (guard, U)) where
  n = length strs - 1
  blocks = S.fromList [ (r, c) | r <- [0..n], c <- [0..n], strs !! r !! c == '#']
  guard = head [(r, c) | r <- [0..n], c <- [0..n], strs !! r !! c == '^']