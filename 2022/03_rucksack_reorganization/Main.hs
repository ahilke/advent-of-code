module Main where

import Constants (priority)
import Paths_advent_of_code (getDataFileName)
import Read (readLines)

import Data.List (find)
import Data.List.Split (chunksOf)
import Data.Maybe (fromJust)

findDuplicate :: [Char] -> [Char] -> Char
findDuplicate a b = fromJust $ find searchChar a
  where
    searchChar char = char `elem` b

splitList :: [a] -> ([a], [a])
splitList list = splitAt half list
  where
    half = div (length list) 2

findBadge :: [Char] -> [Char] -> [Char] -> Char
findBadge a b c = fromJust $ find searchChar a
  where
    searchChar char = elem char b && elem char c

main :: IO ()
main = do
    input <- readLines (getDataFileName "2022/03_rucksack_reorganization/input.txt")
    let compartments = map splitList input
    let duplicates = map mapper compartments
          where
            mapper (a, b) = findDuplicate a b
    let priorities = map priority duplicates
    print $ sum priorities
    let badgeGroups = chunksOf 3 input
    let badges = map mapper badgeGroups
          where
            mapper (a:b:c:_) = findBadge a b c
            mapper _ = error "error mapping badge groups to badges"
    print $ sum $ map priority badges
