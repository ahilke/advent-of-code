module Main where

import Paths_advent_of_code (getDataFileName)
import Read (readLines)

import Data.Char (digitToInt)
import Data.List (find, findIndex)
import Data.Matrix (Matrix, fromLists, getCol, getElem, getRow, mapPos)
import Data.Maybe (isNothing)
import Data.Vector (toList)

biggerThan :: [Int] -> Int -> Bool
biggerThan list x = isNothing biggerTree
  where
    biggerTree = find (>= x) list

biggestTreeFromTop :: Matrix Int -> (Int, Int) -> Bool
biggestTreeFromTop treeMap (iRow, iCol) = biggerThan topColumn tree
  where
    tree = getElem iRow iCol treeMap
    column = getCol iCol treeMap
    topColumn = take (iRow - 1) (toList column)

biggestTreeFromBottom :: Matrix Int -> (Int, Int) -> Bool
biggestTreeFromBottom treeMap (iRow, iCol) = biggerThan bottomColumn tree
  where
    tree = getElem iRow iCol treeMap
    column = getCol iCol treeMap
    bottomColumn = drop iRow (toList column)

biggestTreeFromLeft :: Matrix Int -> (Int, Int) -> Bool
biggestTreeFromLeft treeMap (iRow, iCol) = biggerThan leftRow tree
  where
    tree = getElem iRow iCol treeMap
    row = getRow iRow treeMap
    leftRow = take (iCol - 1) (toList row)

biggestTreeFromRight :: Matrix Int -> (Int, Int) -> Bool
biggestTreeFromRight treeMap (iRow, iCol) = biggerThan rightRow tree
  where
    tree = getElem iRow iCol treeMap
    row = getRow iRow treeMap
    rightRow = drop iCol (toList row)

isVisible :: Matrix Int -> (Int, Int) -> Bool
isVisible treeMap pos =
    biggestTreeFromTop treeMap pos ||
    biggestTreeFromBottom treeMap pos || biggestTreeFromLeft treeMap pos || biggestTreeFromRight treeMap pos

boolToInt :: Bool -> Int
boolToInt True = 1
boolToInt False = 0

viewRange :: Int -> [Int] -> Int
viewRange height trees =
    case findIndex (>= height) trees of
        Just value -> min (value + 1) (length trees)
        Nothing -> length trees

scenicScore :: Matrix Int -> (Int, Int) -> Int
scenicScore treeMap (iRow, iCol) =
    viewRange tree topColumn * viewRange tree bottomColumn * viewRange tree leftRow * viewRange tree rightRow
  where
    tree = getElem iRow iCol treeMap
    column = toList $ getCol iCol treeMap
    topColumn = reverse $ take (iRow - 1) column -- need to reverse to go from tree to the top
    bottomColumn = drop iRow column
    row = toList $ getRow iRow treeMap
    leftRow = reverse $ take (iCol - 1) row -- need to reverse to go from tree to the left
    rightRow = drop iCol row

main :: IO ()
main = do
    rawInput <- readLines (getDataFileName "2022/08_treetop_tree_house/input.txt")
    let input = map (map digitToInt) rawInput
    let treeMap = fromLists input
    let visibleTreeMap = mapPos checkVisible treeMap
          where
            checkVisible (iRow, iCol) _ = isVisible treeMap (iRow, iCol)
    let visibleCount = foldl countTrue 0 visibleTreeMap
          where
            countTrue acc bool = acc + boolToInt bool
    print visibleCount
    let scenicTreeMap = mapPos getScenicScore treeMap
          where
            getScenicScore (iRow, iCol) _ = scenicScore treeMap (iRow, iCol)
    let maxScenicScore = foldl max 0 scenicTreeMap
    print maxScenicScore
