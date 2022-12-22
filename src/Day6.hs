module Day6 where
import qualified Data.Text as T
import Data.List.Split as D
import Data.List as L

-- Main function works for part 1 & 2

-- Part 1: Initialize firstIndex = findStartingIndex contents 4
-- Part 2: Initialise firstIndex = findStartingIndex2 contents 14

main :: IO ()
main = do
      contents <- readFile "day6.txt"
      let firstIndex = findStartingIndex2 contents 14
      print firstIndex

findStartingIndex :: String -> Int -> Int
findStartingIndex (a:b:c:d:xs) n | a /= b && a /=c && b /= d && b /= c && c /= d && a /= d = n
                                 | otherwise = findStartingIndex (b:c:d:xs) (n+1)

findStartingIndex2 :: String -> Int -> Int
findStartingIndex2 (x:xs) n | areTheyAllDifferent (take 14 (x:xs)) == True = n
                            | areTheyAllDifferent (take 14 (x:xs)) == False = findStartingIndex2 xs (n+1)
                            | otherwise = findStartingIndex2 xs (n+1)

areTheyAllDifferent :: String -> Bool
areTheyAllDifferent xs | length (checker xs []) == 14 = True
                       | otherwise = False

checker :: String -> String -> String
checker [] ys = ys
checker (x:xs) ys = checker xs ([ y | y <- [x], not (y `elem` xs)] ++ ys)