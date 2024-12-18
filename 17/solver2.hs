import Text.Regex.TDFA ( (=~), AllMatches(getAllMatches), AllTextMatches (getAllTextMatches) )
import Prelude hiding (lookup)
import Data.List.Split (splitOn)
import Data.Maybe (fromJust, isNothing, isJust)

data State = State {
  ip :: Int,
  program :: [Int],
  regA :: Int,
  regB :: Int,
  regC :: Int,
  outputs :: [Int]
} deriving (Show)

main :: IO ()
main = do
  contents <- getContents
  let strs = lines contents
  let initState = parseState strs
  let prg = program initState
  let regABin = fromJust $ findBinRec initState [] (reverse $ program initState)
  print $ bin2Num regABin
  let stateWithA = initState{regA=(bin2Num regABin)}
  print $ run stateWithA

findBinRec :: State -> [Int] -> [Int] -> Maybe [Int]
findBinRec _ bin [] = Just bin
findBinRec state bin (n:ns)
  | null matches = Nothing
  | all isNothing results = Nothing
  | otherwise = head $ filter isJust results where
  nextThrees = [[a,b,c] | a <- [0,1], b <- [0,1], c <- [0,1]]
  nextBins = [bin ++ nextThree | nextThree <- nextThrees]
  outputs = map (getFirstOutputForInput state) nextBins
  matches = map fst $ filter (\(b, o') -> o'== n) $ zip nextBins outputs
  results = [findBinRec state match ns | match <- matches]

getFirstOutputForInput :: State -> [Int] -> Int
getFirstOutputForInput state bin = head $ outputs state' where
  a = bin2Num bin
  stateWithA = state{regA=a}
  state' = run stateWithA

run :: State -> State
run state
  | pc >= length (program state) = state
  | opcode == 0 = run $ state{ip=pc+2,regA=regA state `div` 2^combo}
  | opcode == 1 = run $ state{ip=pc+2,regB=bxor (regB state) lit}
  | opcode == 2 = run $ state{ip=pc+2,regB=combo `mod` 8}
  | opcode == 3 = run $ if regA state == 0 then state{ip=pc+2} else state{ip=lit}
  | opcode == 4 = run $ state{ip=pc+2,regB=bxor (regB state) (regC state)}
  | opcode == 5 = run $ state{ip=pc+2,outputs=outputs state ++ [combo `mod` 8]}
  | opcode == 6 = run $ state{ip=pc+2,regB=regA state `div` 2^combo}
  | opcode == 7 = run $ state{ip=pc+2,regC=regA state `div` 2^combo}
  | otherwise = error "unknow opcode"
  where
    pc = ip state
    opcode = program state !! pc
    lit = program state !! (pc+1)
    combo = case lit of
      4 -> regA state
      5 -> regB state
      6 -> regC state
      _ -> lit

bxor :: Int -> Int -> Int
bxor a b = bin2Num $ reverse $ bxor' (reverse $ num2Bin a) (reverse $ num2Bin b)

bxor' :: [Int] -> [Int] -> [Int]
bxor' [] [] = []
bxor' [] bs = bs
bxor' as [] = as
bxor' (a:as) (b:bs) = (if a /= b then 1 else 0) : bxor' as bs

bin2Num :: [Int] -> Int
bin2Num = foldl (\acc b -> 2*acc + b) 0

num2Bin :: Int -> [Int]
num2Bin 0 = [0]
num2Bin n = reverse (num2Bin' n) where
  num2Bin' 0 = []
  num2Bin' n' = (n' `mod` 2) : num2Bin' (n' `div` 2)

parseState :: [String] -> State
parseState strs = State 0 prg (read a) (read b) (read c) [] where
  [regStrs, prgStrs] = splitOn [""] strs
  regex = "[0-9]+"
  a = regStrs !! 0 =~ regex :: String
  b = regStrs !! 1 =~ regex :: String
  c = regStrs !! 2 =~ regex :: String
  prgStr = splitOn " " (head prgStrs) !! 1
  prg = map read $ splitOn "," prgStr
