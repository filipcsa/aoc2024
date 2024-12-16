import Text.Regex.TDFA ( (=~), AllMatches(getAllMatches), AllTextMatches (getAllTextMatches) )
import Prelude hiding (lookup)
import qualified Data.PQueue.Prio.Min as Q
import qualified Data.Map as M

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
  let open = M.singleton startPos 0
  let cost = dijkstra strs q open
  print cost

dijkstra :: [String] -> Q.MinPQueue Int ((Int, Int), Ori) -> M.Map ((Int, Int), Ori) Int -> Int
dijkstra strs q open 
  | isTarget strs pos = c
  | otherwise = dijkstra strs q' open' where
    (c, pos) = Q.findMin q
    q0 = Q.deleteMin q
    rotSuccs = getRotSuccs (c, pos)
    forwardSucc = getForwardSucc (c, pos)
    allSuccs = filter (\(_, pos) -> isValidPos strs pos) 
      $ filter (\(c, pos) -> c < M.findWithDefault 1000000 pos open) (forwardSucc : rotSuccs)
    q' = foldl (\acc (c, pos) -> Q.insert c pos acc) q0 allSuccs
    open' = foldl(\acc (c, pos) -> M.insert pos c acc) open allSuccs

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
