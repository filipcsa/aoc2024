import Text.Regex.TDFA ( (=~), AllMatches(getAllMatches), AllTextMatches (getAllTextMatches) )
import Prelude hiding (lookup)

main :: IO ()
main = do
  contents <- getContents
  let inputLine = concat $ lines contents
  let regex = "mul\\([0-9]+,[0-9]+\\)|do\\(\\)|don't\\(\\)"
  let matches = getAllTextMatches (inputLine =~ regex) :: [String]
  let res = foldl sumProdIfEnabled (0, True) matches
  print res

sumProdIfEnabled :: (Int, Bool) -> String -> (Int, Bool)
sumProdIfEnabled (s, e) str
  | str == "do()" = (s, True)
  | str == "don't()" = (s, False)
  | not e = (s, e)
  | e = (s + p, e) where
    (a, b) = matchPair str
    p = a * b

matchPair :: String -> (Int, Int)
matchPair str = (read a, read b) where
  [a, b] = getAllTextMatches (str =~ "[0-9]+") :: [String]