import Text.Regex.TDFA ( (=~), AllMatches(getAllMatches), AllTextMatches (getAllTextMatches) )
import Prelude hiding (lookup)
import qualified Data.PQueue.Prio.Min as Q
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Function (on)
import Data.List (minimumBy, nub)

data Ori = E | W | S | N deriving (Show, Eq, Ord)
type Coord = (Int, Int)
type Pos = (Coord, Ori)

main :: IO ()
main = do
  contents <- getContents
  let strs = lines contents
  let startCoords = head [(x,y) | x <- [0..length strs - 1], y <- [0..length strs - 1], strs !! x !! y == 'S']
  let startPos = (startCoords, E)
  let q = Q.singleton 0 startPos
  let costMap = M.singleton startPos 0
  let predMap = dijkstra strs q costMap 1000000 M.empty
  let end = minimumBy (compare `on` fst) $ filter (\(_, ((x,y), _)) -> strs !! x !! y == 'E') $ M.keys predMap
  let path = reconstructPath predMap (S.singleton end) end
  print $ length $ nub $ map (\(_, ((x,y), _)) -> (x, y)) $ S.toList path

reconstructPath :: M.Map (Int, Pos) [(Int, Pos)] -> S.Set (Int, Pos) -> (Int, Pos) -> S.Set (Int, Pos)
reconstructPath predMap visited succ
  | M.notMember succ predMap = visited
  | otherwise = foldl (\acc pos -> S.union acc (reconstructPath predMap acc pos)) visited' preds where
    preds = filter (`S.notMember` visited) $ M.findWithDefault [] succ predMap
    visited' = foldl (flip S.insert) visited preds

dijkstra :: [String] -> Q.MinPQueue Int Pos -> M.Map Pos Int -> Int -> M.Map (Int, Pos) [(Int, Pos)] -> M.Map (Int, Pos) [(Int, Pos)]
dijkstra strs q costMap ub predMap
  | all (> ub) $ Q.keys q = predMap
  | otherwise = dijkstra strs q' costMap' ub' predMap' where
    (c, pos) = Q.findMin q
    q0 = Q.deleteMin q
    rotSuccs = getRotSuccs (c, pos)
    forwardSucc = getForwardSucc (c, pos)
    allSuccs = filter (\(_, pos) -> isValidPos strs pos)
      $ filter (\(c, pos) -> c <= M.findWithDefault 1000000 pos costMap) (forwardSucc : rotSuccs)
    q' = foldl (\acc (c, pos) -> Q.insert c pos acc) q0 allSuccs
    costMap' = foldl (\acc (c, pos) -> M.insert pos c acc) costMap allSuccs
    ub' = if strs !! x !! y == 'E' then c else ub where ((x, y), _) = pos
    predMap' = foldl (updatePredMap (c, pos) costMap) predMap allSuccs

updatePredMap :: (Int, Pos) -> M.Map Pos Int -> M.Map (Int, Pos) [(Int, Pos)] -> (Int, Pos) -> M.Map (Int, Pos) [(Int, Pos)]
updatePredMap pred costMap predMap succ = M.insert succ preds predMap where
  preds0 = M.findWithDefault [] succ predMap
  preds = pred : preds0

isValidPos :: [String] -> ((Int, Int), Ori) -> Bool
isValidPos strs ((x,y), _)= strs !! x !! y /= '#'

getForwardSucc :: (Int, ((Int, Int), Ori)) -> (Int, ((Int, Int), Ori))
getForwardSucc (c, ((x,y), ori)) = (c+1, moveForward ((x,y), ori))

moveForward :: ((Int, Int), Ori) -> ((Int, Int), Ori)
moveForward ((x,y), E) = ((x, y+1), E)
moveForward ((x,y), W) = ((x, y-1), W)
moveForward ((x,y), S) = ((x+1, y), S)
moveForward ((x,y), N) = ((x-1, y), N)

getRotSuccs :: (Int, ((Int, Int), Ori)) -> [(Int, ((Int, Int), Ori))]
getRotSuccs (c, ((x,y), ori)) = [
  (c+1000, ((x,y), rotL ori)),
  (c+1000, ((x,y), rotR ori))]

rotR :: Ori -> Ori
rotR E = S
rotR S = W
rotR W = N
rotR N = E

rotL :: Ori -> Ori
rotL E = N
rotL N = W
rotL W = S
rotL S = E

isTarget :: [String] -> ((Int, Int), Ori) -> Bool
isTarget strs ((x, y), _)= strs !! x !! y == 'E'
