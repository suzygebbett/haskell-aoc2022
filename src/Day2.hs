module Day2 where
import qualified Data.Text as T
import Data.List.Split as D
import Data.List as L

main :: IO ()
main = do 
      contents <- readFile "day2.txt"
      let elves = D.splitOn "\n" contents
      let result = getScore elves 0
      print result
      
-- Part 1 --

getScore :: [String] -> Int -> Int
getScore [] n = n
getScore (x:xs) n | x == "A X" = getScore xs (n+4)
                  | x == "A Y" = getScore xs (n+8)
                  | x == "A Z" = getScore xs (n+3)
                  | x == "B X" = getScore xs (n+1)
                  | x == "B Y" = getScore xs (n+5)
                  | x == "B Z" = getScore xs (n+9)
                  | x == "C X" = getScore xs (n+7)
                  | x == "C Y" = getScore xs (n+2)
                  | x == "C Z" = getScore xs (n+6)
                  | otherwise = error "Incorrect input values!"

-- Part 2 --

-- Replace "getScore" with "getScore2" in main to get the answer for part 2.

getScore2 :: [String] -> Int -> Int
getScore2 [] n = n
getScore2 (x:xs) n | x == "A X" = getScore2 xs (n+3)
                  | x == "A Y" = getScore2 xs (n+4)
                  | x == "A Z" = getScore2 xs (n+8)
                  | x == "B X" = getScore2 xs (n+1)
                  | x == "B Y" = getScore2 xs (n+5)
                  | x == "B Z" = getScore2 xs (n+9)
                  | x == "C X" = getScore2 xs (n+2)
                  | x == "C Y" = getScore2 xs (n+6)
                  | x == "C Z" = getScore2 xs (n+7)
                  | otherwise = error "Incorrect input values!"
