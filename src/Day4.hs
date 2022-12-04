module Day4 where
import qualified Data.Text as T
import Data.List.Split as D
import Data.List as L
-- Main function works for part 1 & 2, replace isItOverlapping with isItContained for Part 1 answer
main :: IO ()
main = do 
      contents <- readFile "day4.txt"
      let cleaning = D.splitOn "\n" contents
      let pairs = concat (map (D.splitOn ",") cleaning)
      let each = concat (map (D.splitOn "-") pairs)
      let result = isItOverlapping each 0
      print result   
-- Part 1
isItContained :: [String] -> Int -> Int
isItContained [] n = n
isItContained (x:y:z:a:xs) n | (read x :: Int) <= (read z :: Int) && (read y :: Int) >= (read a :: Int) = isItContained xs (n+1)
                             | (read x :: Int) >= (read z :: Int) && (read y :: Int) <= (read a :: Int) = isItContained xs (n+1)
                             | otherwise = isItContained xs n
-- Part 2
isItOverlapping :: [String] -> Int -> Int
isItOverlapping [] n = n
isItOverlapping (x:y:z:a:xs) n | (read y :: Int) < (read z :: Int) = isItOverlapping xs (n)
                               | (read x :: Int) > (read a :: Int) = isItOverlapping xs (n)
                               | otherwise = isItOverlapping xs (n+1)