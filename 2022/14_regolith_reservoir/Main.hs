module Main where

import Paths_advent_of_code (getDataFileName)
import Read (readLines)

import Data.List.Split (splitOn)
import Data.Matrix (Matrix, getElem, matrix, safeGet, setElem, submatrix)
import Data.Maybe (fromJust, isNothing)
import Data.Tuple (swap)
import Text.Printf (printf)

-- | Where the sand starts pouring in as (col, row) starting with index 1.
sandStart :: (Int, Int)
sandStart = (500, 1)

air :: Char
air = ' '

rock :: Char
rock = '#'

sand :: Char
sand = 'o'

processLine :: String -> [(Int, Int)]
processLine line = map toIntTuple stringPairs
  where
    stringTuples = splitOn " -> " line
    stringPairs = map (splitOn ",") stringTuples

toIntTuple :: [String] -> (Int, Int)
toIntTuple [x, y] = (read x, read y)
toIntTuple x = error $ printf "Unexpected input %s" (show x)

addRockPath :: Matrix Char -> [(Int, Int)] -> Matrix Char
addRockPath caveMap (current:next:rest) = addRockPath (addRockLine caveMap current next) (next : rest)
addRockPath caveMap _ = caveMap

addRockLine :: Matrix Char -> (Int, Int) -> (Int, Int) -> Matrix Char
addRockLine caveMap (currentCol, currentRow) (targetCol, targetRow)
    | currentCol == targetCol && currentRow == targetRow = updatedMap
    | otherwise = addRockLine updatedMap newPos (targetCol, targetRow)
  where
    newPos = moveTowards (currentCol, currentRow) (targetCol, targetRow)
    updatedMap = setElem rock (currentRow, currentCol) caveMap

moveTowards :: (Int, Int) -> (Int, Int) -> (Int, Int)
moveTowards (currentCol, currentRow) (targetCol, targetRow)
    | currentCol == targetCol && currentRow < targetRow = (currentCol, currentRow + 1)
    | currentCol == targetCol && currentRow > targetRow = (currentCol, currentRow - 1)
    | currentRow == targetRow && currentCol < targetCol = (currentCol + 1, currentRow)
    | currentRow == targetRow && currentCol > targetCol = (currentCol - 1, currentRow)
    | otherwise =
        error $ printf "Cannot move from %s towards %s!" (show (currentCol, currentRow)) (show (targetCol, targetRow))

simulateSand :: (Matrix Char, Int) -> (Matrix Char, Int)
simulateSand (caveMap, sandUnits) =
    if simulationRunning
        then simulateSand (newCaveMap, sandUnits + 1)
        else (caveMap, sandUnits)
  where
    (newCaveMap, simulationRunning) = simulateSandUnit caveMap sandStart

-- | Returns the updated map and whether the sand unit came to rest or fell off.
simulateSandUnit :: Matrix Char -> (Int, Int) -> (Matrix Char, Bool)
simulateSandUnit caveMap (sandCol, sandRow)
    | isNothing down = (caveMap, False)
    | left /= air && safeDown /= air && right /= air = (setElem sand (sandRow, sandCol) caveMap, True)
    | safeDown == air = simulateSandUnit caveMap (sandCol, sandRow + 1)
    | left == air = simulateSandUnit caveMap (sandCol - 1, sandRow + 1)
    | right == air = simulateSandUnit caveMap (sandCol + 1, sandRow + 1)
    | otherwise = error "Unexpected case!"
  where
    down = safeGet (sandRow + 1) sandCol caveMap
    safeDown = fromJust down
    left = getElem (sandRow + 1) (sandCol - 1) caveMap
    right = getElem (sandRow + 1) (sandCol + 1) caveMap

main :: IO ()
main = do
    input <- readLines (getDataFileName "2022/14_regolith_reservoir/input.txt")
    let emptyMap = matrix 500 1000 (const air)
    let mapWithStart = setElem '+' (swap sandStart) emptyMap -- TODO: why is col/row inversed for set?
    let mapWithRocks = foldl parseLineAndAddPath mapWithStart input
          where
            parseLineAndAddPath caveMap line = addRockPath caveMap (processLine line)
    let (mapWithSand, steps) = simulateSand (mapWithRocks, 0)
    print $ submatrix 1 20 490 510 mapWithSand
    print steps
