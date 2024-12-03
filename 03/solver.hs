import Text.Regex.TDFA ( (=~), AllMatches(getAllMatches), AllTextMatches (getAllTextMatches) )
import Prelude hiding (lookup)

main :: IO ()
main = do
  contents <- getContents
  let inputLine = concat $ lines contents
  let regex = "mul\\([0-9]+,[0-9]+\\)"
  let muls = getAllTextMatches (inputLine =~ regex) :: [String]
  let numPairsToMul = map matchPair muls
  let result = sum $ map (uncurry (*)) numPairsToMul
  print result

matchPair :: String -> (Int, Int)
matchPair str = (read a, read b) where
  [a, b] = getAllTextMatches (str =~ "[0-9]+") :: [String]