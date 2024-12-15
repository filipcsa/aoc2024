import Text.Regex.TDFA ( (=~), AllMatches(getAllMatches), AllTextMatches (getAllTextMatches) )
import Prelude hiding (lookup)
import qualified Data.Set as S
import Data.List.Split (splitOn)

type Pos = (Int, Int)
data State = State {
  robot :: Pos,
  boxes :: S.Set Pos,
  walls :: S.Set Pos,
  moves :: [Char]
} deriving Show

main :: IO ()
main = do
  contents <- getContents
  let inputLines = lines contents
  let initState = parseState inputLines
  let finalState = execMoves initState
  print $ sum $ map gps (S.toList $ boxes finalState)

gps :: Pos -> Int
gps (x,y) = 100*x + y

execMoves :: State -> State
execMoves state
  | null (moves state) = state
  | otherwise = execMoves state{robot=robot',boxes=boxes',moves=moves'} where
    (move:moves') = moves state
    (robot', movedBoxes) = execMove move state (robot state) []
    boxes' = foldl (\acc b -> S.insert (doMove move b) $ S.delete b acc) (boxes state) movedBoxes

execMove :: Char -> State -> Pos -> [Pos] -> (Pos, [Pos])
execMove move state pos movedBoxes
  | pos `S.member` walls state = (robot state, [])
  | emptyTile = (doMove move (robot state), movedBoxes)
  | otherwise = execMove move state (doMove move pos) movedBoxes' where
    emptyTile = pos /= robot state && pos `S.notMember` boxes state
    movedBoxes' = if pos `S.member` boxes state then pos:movedBoxes else movedBoxes

doMove :: Char -> Pos -> Pos
doMove '>' (x, y) = (x, y+1)
doMove '<' (x, y) = (x, y-1)
doMove '^' (x, y) = (x-1, y)
doMove 'v' (x, y) = (x+1, y)


parseState :: [String] -> State
parseState strs = State robot boxes walls moves where
  [l, ms] = splitOn [""] strs
  h = length l
  w = length (head l)
  robot = head [(x,y) | x <- [0..h-1], y <- [0..w-1], l !! x !! y == '@']
  boxes = S.fromList [(x,y) | x <- [0..h-1], y <- [0..w-1], l !! x !! y == 'O']
  walls = S.fromList [(x,y) | x <- [0..h-1], y <- [0..w-1], l !! x !! y == '#']
  moves = concat ms
