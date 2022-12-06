module Main where

import Paths_advent_of_code (getDataFileName)
import Read (readLines)

import Data.List (nub)

allDifferent :: String -> Bool
allDifferent chars = length (nub chars) == length chars

findStart :: Int -> [Char] -> Int -> Int
findStart requiredDifferent chars count =
    if allDifferent (take requiredDifferent chars)
        then count
        else findStart requiredDifferent (tail chars) (count + 1)

main :: IO ()
main = do
    input <- readLines (getDataFileName "2022/06_tuning_trouble/input.txt")
    let firstLine = head input
    let packetStart = findStart 4 firstLine 4
    print packetStart
    let messageStart = findStart 14 firstLine 14
    print messageStart
