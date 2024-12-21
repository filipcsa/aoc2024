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
  let ls = map shortestSeqLength inputLines
  let ns = map (read . init) inputLines :: [Int]
  print $ sum $ zipWith (*) ls ns

shortestSeqLength :: String -> Int
shortestSeqLength str = minLength3 where
  seqs1 = moveSeqs numpad str

  seqs2' = concatMap (moveSeqs dirpad) seqs1
  minLength2 = minimum $ map length seqs2'
  seqs2 = filter (\s -> length s == minLength2) seqs2'

  seqs3' = concatMap (moveSeqs dirpad) seqs2
  minLength3 = minimum $ map length seqs3'

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