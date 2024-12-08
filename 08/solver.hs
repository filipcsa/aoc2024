import Prelude hiding (lookup)
import qualified Data.Map as M
import Data.List (nub)

type Pos = (Int, Int)
type Antennas = M.Map Char [Pos]

main :: IO ()
main = do
  contents <- getContents
  let inputLines = lines contents
  let antennas = parseAntennas inputLines :: Antennas
  let mapSize = length inputLines
  let allAntinodes = concatMap (buildAntinodes mapSize . snd) (M.toList antennas)
  let validAntinodes = nub $ filter (validPos mapSize) allAntinodes
  print $ length validAntinodes

validPos :: Int -> Pos -> Bool
validPos s (r,c) = r >= 0 && r < s && c >= 0 && c < s

buildAntinodes :: Int -> [Pos] -> [Pos]
buildAntinodes s poss = concat [ buildAntinode s a b | a <- poss, b <- poss, a /= b]

buildAntinode :: Int -> Pos -> Pos -> [Pos]
buildAntinode s (a1, a2) (b1, b2) = takeWhile (validPos s) [(a1 - d1*i, a2 - d2*i) | i <- [0..]] where
  (d1, d2) = (b1 - a1, b2 - a2)


parseAntennas :: [String] -> Antennas
parseAntennas strs =
  foldl (parseAntenna strs) M.empty [(r,c) | r <- [0..s], c <- [0..s]] where
    s = length strs - 1

parseAntenna :: [String] -> Antennas -> Pos -> Antennas
parseAntenna strs antennas (r,c) =  if charAtPos == '.'
  then antennas
  else M.insert charAtPos updatedList antennas where
    charAtPos = strs !! r !! c
    l = M.findWithDefault [] charAtPos antennas
    updatedList = l ++ [(r,c)]
