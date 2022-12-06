module Main where

import Paths_advent_of_code (getDataFileName)
import Read (readLines)

allDifferent :: Char -> Char -> Char -> Char -> Bool
allDifferent a b c d = a /= b && a /= c && a /= d && b /= c && b /= d && c /= d

findStart :: [Char] -> Int -> Int
findStart (a:b:c:d:rest) count =
    if allDifferent a b c d
        then count
        else findStart (b : c : d : rest) (count + 1)
findStart _ _ = error "no start pattern found"

main :: IO ()
main = do
    input <- readLines (getDataFileName "2022/06_tuning_trouble/input.txt")
    let start = findStart (head input) 4
    print start
