module Day3 where
import qualified Data.Text as T
import Data.List.Split as D
import Data.List as L

-- Main function works for part 1 & 2

main :: IO ()
main = do 
      contents <- readFile "day3.txt"
      let rucksack = D.splitOn "\n" contents
      let result = valueTotal rucksack
      print result
      

splitLists :: [String] -> [Char] -> [Char]
splitLists [] n = n
splitLists (x:xs) n =  splitLists xs (getCharacter (splitAt ((length x) `div` 2) x) ++ n)


getCharacter :: ([Char], [Char]) -> [Char]
getCharacter (x, y) = [head [n | n <- x, n `elem` y]]

findChar :: Char -> [(Char, Int)] -> Int
findChar n ((x, y):xs) | x == n = y
                       | otherwise = findChar n xs
              
              
valueChar :: [(Char, Int)]
valueChar = zip (['a'..'z'] ++ ['A'..'Z'])[1..]

-- Change valueTotal function to get answers for 1 or 2
-- Part 1: use splitLists
-- Part 2: getCharacter2

valueTotal :: [String] -> Int
valueTotal xs = sum [ findChar (n) (valueChar) | n <- (getCharacter2 xs [])]

getCharacter2 :: [String] -> [Char] -> [Char]
getCharacter2 [] m = m
getCharacter2 (x:y:z:xs) m = getCharacter2 xs ([head [n | n <- x, n `elem` y && n `elem`z]] ++ m)