import qualified Data.Map as M
import Data.List (nub, permutations)

type Pos = (Int, Int)

numpad :: M.Map Char Pos
numpad = M.fromList [(strs !! x !! y, (x,y)) | x <- [0..3], y <- [0..2]] where
  strs = ["789", "456", "123"," 0A"]

dirpad :: M.Map Char Pos
dirpad = M.fromList [(strs !! x !! y, (x,y)) | x <- [0,1], y <- [0..2]] where
  strs = [" ^A", "<v>"]

main :: IO ()
main = do
  contents <- getContents
  let inputLines = lines contents
  let seqPerPair = buildDPSeqPairs
  let ls = map (shortestSeqLength seqPerPair 2) inputLines
  let ns = map (read . init) inputLines :: [Int]
  print ls
  print ns
  print $ sum $ zipWith (*) ls ns

shortestSeqLength :: M.Map (Char, Char) String -> Int -> String -> Int
shortestSeqLength dirPairs n str = minLength where
  seqs0 = moveSeqs numpad str
  seqPairs = map (\s -> zip ('A':s) s) seqs0
  pairOccs = map (foldl (\acc p -> M.insert p (1 + M.findWithDefault 0 p acc) acc) M.empty) seqPairs
  minLength = minimum $ map (dirSeqSim dirPairs n) pairOccs

dirSeqSim :: M.Map (Char, Char) String -> Int -> M.Map (Char, Char) Int -> Int
dirSeqSim _ 0 seqOccs = sum $ M.elems seqOccs
dirSeqSim dirSeqs n seqOccs = dirSeqSim dirSeqs (n-1) seqOccs' where
  seqOccs' = foldl (accumPairs dirSeqs) M.empty (M.toList seqOccs)

accumPairs :: M.Map (Char, Char) String -> M.Map (Char, Char) Int -> ((Char, Char), Int) -> M.Map (Char, Char) Int
accumPairs dirPairs pairOccs (pair, occs) = 
  foldl (\acc p -> M.insert p (occs + M.findWithDefault 0 p acc) acc) pairOccs newPairs where
    newSeq = M.findWithDefault "" pair dirPairs
    newPairs = if head newSeq /= 'A'
      then zip ('A':newSeq) newSeq
      else zip newSeq (tail newSeq)

buildDPSeqPairs :: M.Map (Char, Char) String
buildDPSeqPairs = M.fromList [((prev, next), head $ moveSeqs' dirpad (prev, next))
  | prev <- "^A<v>", next <- "^A<v>"]

moveSeqs :: M.Map Char Pos -> String -> [String]
moveSeqs somepad str = foldl (\acc ps -> [a ++ p | a <- acc, p <- ps]) [""] subseqPerms where
  subseqPerms = map (moveSeqs' somepad) $ zip ('A':str) str

moveSeqs' :: M.Map Char Pos -> (Char, Char) -> [String]
moveSeqs' somepad (prev, next) = map (++ "A") validSeqs where
  validSeqs = filter (isValidSeq (prevX, prevY) forbidden) uniqSeqs
  uniqSeqs = nub $ permutations vhmoves
  vhmoves = vMove prevX nextX ++ hMove prevY nextY
  (prevX, prevY) = M.findWithDefault (0,0) prev somepad
  (nextX, nextY) = M.findWithDefault (0,0) next somepad
  forbidden = M.findWithDefault (0,0) ' ' somepad

isValidSeq :: Pos -> Pos -> [Char] -> Bool
isValidSeq _ _ [] = True
isValidSeq (x,y) forbidden (m:ms) = (x,y) /= forbidden && isValidSeq pos' forbidden ms where
  pos' = case m of
    '>' -> (x,y+1)
    '<' -> (x,y-1)
    '^' -> (x-1,y)
    'v' -> (x+1,y)

hMove :: Int -> Int -> [Char]
hMove prev next | prev < next = replicate (next-prev) '>'
                | next < prev = replicate (prev-next) '<'
                | otherwise = []

vMove :: Int -> Int -> [Char]
vMove prev next | prev < next = replicate (next-prev) 'v'
                | next < prev = replicate (prev-next) '^'
                | otherwise = []