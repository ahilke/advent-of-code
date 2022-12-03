module Main where

import Constants (priority)
import Paths_advent_of_code (getDataFileName)
import Read (readLines)

import Data.List (elemIndex, find)
import Data.Maybe (fromJust, isJust)

findDuplicate :: [Char] -> [Char] -> Char
findDuplicate a b = fromJust $ find searchChar a
  where
    searchChar char = isJust $ elemIndex char b

splitList :: [a] -> ([a], [a])
splitList list = splitAt half list
  where
    half = div (length list) 2

main :: IO ()
main = do
    input <- readLines (getDataFileName "2022/03_rucksack_reorganization/input.txt")
    let compartments = map splitList input
    let duplicates = map mapper compartments
          where
            mapper (a, b) = findDuplicate a b
    let priorities = map priority duplicates
    print $ sum priorities
