module Main where

import Data.List (sort)
import Data.List.Split (splitOn)
import Paths_advent_of_code (getDataFileName)

readLines :: IO FilePath -> IO [String]
readLines path = do filePath <- path
                    contents <- readFile filePath
                    return (lines contents)

toInt :: [String] -> [Int]
toInt strings = [read string | string <- strings]

main :: IO ()
main = do input <- readLines (getDataFileName "2022/01_adding/input.txt")
          let inventories = splitOn [""] input
          let parsedInventories = map toInt inventories
          let sortedSums = reverse $ sort $ map sum parsedInventories

          let result1 = head sortedSums
          let result2 = sum $ take 3 sortedSums

          print result1
          print result2
