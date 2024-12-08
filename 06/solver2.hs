import Prelude hiding (lookup)
import qualified Data.Set as S
import Data.List (nub)
import Language.Haskell.TH (doE)

type Pos = (Int, Int)
data Ori = U | D | L | R deriving (Eq, Ord, Show)

main :: IO ()
main = do
  contents <- getContents
  let inputLines = lines contents
  let mapSize = length inputLines
  let (blocks, guard) = initializeWorld inputLines
  let guardRoute = takeWhile (not . guardOutsideMap mapSize) $ iterate (simulateGuard mapSize blocks) guard
  print $ length $ nub $ map fst guardRoute
  let loopCauses = snd $ foldl (checkLoop mapSize blocks) (S.empty, []) guardRoute
  print $ length
    $ filter (\pos -> pos /= fst guard)
    $ nub loopCauses

uniqueRoute :: (S.Set Pos, [(Pos, Ori)]) -> (Pos, Ori) -> (S.Set Pos, [(Pos, Ori)])
uniqueRoute (poss, path) (pos, ori)
  | S.member pos poss = (poss, path)
  | otherwise = (S.insert pos poss, path ++ [(pos, ori)])

checkLoop :: Int -> S.Set Pos -> (S.Set Pos, [Pos]) -> (Pos, Ori) -> (S.Set Pos, [Pos])
checkLoop mapSize blocks (poss, loops) (pos, ori)
  | S.member newBlockPos poss = (poss, loops)
  | hasLoop = (S.insert newBlockPos poss, loops ++ [newBlockPos])
  | otherwise = (S.insert newBlockPos poss, loops)
  where
      newBlockPos = nextPos pos ori
      blocks' = S.insert newBlockPos blocks
      hasLoop = checkLoop' 0 mapSize blocks' S.empty (pos, ori)

checkLoop' :: Int -> Int -> S.Set Pos -> S.Set (Pos, Ori) -> (Pos, Ori) -> Bool
checkLoop' numVisited mapSize blocks visited (pos, ori)
  | guardOnMapEdge mapSize (pos, ori) = False
  | S.member (pos, ori) visited = True -- Why doesnt this work ?
  | otherwise = checkLoop' (numVisited + 1) mapSize blocks visited' (pos', ori') where
    visited' = S.insert (pos, ori) visited
    next = nextPos pos ori
    nextPosBlocked = S.member next blocks
    (pos', ori') = if nextPosBlocked then (pos, updateOri ori) else (next, ori)

guardOnMapEdge :: Int -> (Pos, Ori) -> Bool
guardOnMapEdge s ((r, c), _) = r == 0 || r == (s - 1) || c == 0 || c == (s - 1)

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