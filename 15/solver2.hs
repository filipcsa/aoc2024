import Text.Regex.TDFA ( (=~), AllMatches(getAllMatches), AllTextMatches (getAllTextMatches) )
import Prelude hiding (lookup)
import qualified Data.Set as S
import Data.List.Split (splitOn)

type Pos = (Int, Int)
data State = State {
  robot :: Pos,
  boxes :: S.Set [Pos],
  walls :: S.Set Pos,
  moves :: [Char]
} deriving Show

main :: IO ()
main = do
  contents <- getContents
  let inputLines = lines contents
  let initState = parseState inputLines
  finalState <- execMoves initState
  print $ sum $ map gps (S.toList $ boxes finalState)

gps :: [Pos] -> Int
gps [(x,y), _] = 100*x + y

execMoves :: State -> IO State
execMoves state
  | null (moves state) = return state
  | otherwise = do
    -- print $ "numBoxes " ++ show (length $ boxes state)
    -- printState state
    -- print $ "Move " ++ show move
    (robot', movedBoxes) <- execMove move state [doMove move (robot state)] S.empty
    let boxes0 = foldl (\acc b -> S.delete b acc) (boxes state) (S.toList movedBoxes)
    let boxes' = foldl (\acc b -> S.insert (map (doMove move) b) acc) boxes0 movedBoxes
    execMoves state{robot=robot',boxes=boxes',moves=moves'} where
      (move:moves') = moves state

printState :: State -> IO ()
printState state = sequence_ [print [resolveChar state (x,y) | y <- [0..w]] | x <- [0..h]] where
  h = maximum $ map fst $ S.toList (walls state)
  w = maximum $ map snd $ S.toList (walls state)

resolveChar :: State -> (Int, Int) -> Char
resolveChar state pos
  | pos `S.member` walls state = '#'
  | pos == robot state = '@'
  | any (\b -> head b == pos) (boxes state) = '['
  | any (\b -> last b == pos) (boxes state) = ']'
  | otherwise = '.'


execMove :: Char -> State -> [Pos] -> S.Set [Pos] -> IO (Pos, S.Set [Pos])
execMove move state frontier movedBoxes
  | any (\pos -> pos `S.member` walls state) frontier = do 
    -- print "Hit wall"
    return (robot state, S.empty)
  | emptyTiles = do 
    -- print $ "move finished, moved boxes: " ++ show movedBoxes
    return (doMove move (robot state), movedBoxes)
  | otherwise = do 
    -- print $ "Frontier: " ++ show frontier
    execMove move state (map (doMove move) newFrontier) movedBoxes' where
    unmovedBoxes = S.difference (boxes state) movedBoxes
    unmovedBoxPoss = S.fromList $ concat $ S.toList unmovedBoxes
    emptyTiles = all (`S.notMember` unmovedBoxPoss) frontier
    newBoxesToMove = filter (\[p1, p2] -> p1 `elem` frontier || p2 `elem` frontier) $ S.toList unmovedBoxes
    newFrontier = concat newBoxesToMove
    movedBoxes' = foldl (\acc b -> S.insert b acc) movedBoxes newBoxesToMove

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
  robot = head [(x,2*y) | x <- [0..h-1], y <- [0..w-1], l !! x !! y == '@']
  boxes = S.fromList [[(x,2*y), (x, 2*y+1)] | x <- [0..h-1], y <- [0..w-1], l !! x !! y == 'O']
  walls = S.fromList $ concat [[(x,2*y), (x, 2*y+1)] | x <- [0..h-1], y <- [0..w-1], l !! x !! y == '#']
  moves = concat ms
