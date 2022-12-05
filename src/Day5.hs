module Day5 where
import qualified Data.Text as T
import Data.List.Split as D
import Data.List as L

-- Main function works for part 1 & 2

main :: IO ()
main = do 
      contents <- readFile "day5.txt"
      let oneLine = D.splitOn "\n" contents
      let noMove = concat (map (D.splitOn "move ") oneLine)
      let noFrom = concat (map (D.splitOn " from ") noMove)
      let noTo = concat (map (D.splitOn " to ") noFrom)
      let doIt = getFinal (getLitty stacks [] noTo []) []
      print doIt

getFinal :: [(Int, String)] -> String -> String
getFinal [] ys = ys
getFinal ((x, y):xs) ys = getFinal xs (ys ++ [head (reverse y)])
-- To change between part 1 and part 2, remove or add the final reverse in line 26 
getLitty :: [(Int, String)] -> [(Int, String)] -> [String] -> String -> [(Int, String)]
getLitty xs _ [] _ = xs
getLitty ((x, y):xs) fs (a:b:c:d:zs) [] | x == (read c :: Int) = getLitty (fs ++ ((x, ((take ((length y) - (read b :: Int)) y))):xs)) [] (a:b:c:d:zs) (reverse(take (read b :: Int) (reverse y)))
                                        | otherwise = getLitty xs (fs ++ [(x, y)]) (a:b:c:d:zs) []                                     
getLitty ((x, y):xs) fs (a:b:c:d:zs) n | x == (read d :: Int) = getLitty (fs ++ ((x, (y ++ n)):xs)) [] (zs) []
                                       | otherwise = getLitty xs (fs ++ [(x, y)]) (a:b:c:d:zs) n

stacks :: [(Int, String)]
stacks = [(1, "JHGMZNTF"), (2, "VWJ"), (3, "GVLJBTH"), (4, "BPJNCDVL"), (5, "FWSMPRG"), (6, "GHCFBNVM"), (7, "DHGMR"), (8, "HNMVZD"), (9, "GNFH")]