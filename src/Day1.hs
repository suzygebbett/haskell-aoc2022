module Day1 where
import qualified Data.Text as T
import Data.List.Split as D
import Data.List as L

main :: IO ()
main = do 
      contents <- readFile "day1.txt"
      let elves = D.splitOn "\n\n" contents
      let elves2 = map (D.splitOn "\n") elves
      let elves3 = map (map (read)) elves2
      let elves4 = (L.sort (map sum elves3))
      let elves5 = last (elves4) + head(drop 1 (reverse elves4)) + head(drop 2 (reverse elves4))
      print elves5
