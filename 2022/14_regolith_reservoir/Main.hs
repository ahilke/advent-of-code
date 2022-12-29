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
    updatedMap = setElem rock (currentRow + 1, currentCol) caveMap -- account for 1-index for row where input is 0-indexed to guarantee correct height

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
    | startBlocked = (caveMap, False)
    | isNothing down = (caveMap, False)
    | left /= air && safeDown /= air && right /= air = (setElem sand (sandRow, sandCol) caveMap, True)
    | safeDown == air = simulateSandUnit caveMap (sandCol, sandRow + 1)
    | left == air = simulateSandUnit caveMap (sandCol - 1, sandRow + 1)
    | right == air = simulateSandUnit caveMap (sandCol + 1, sandRow + 1)
    | otherwise = error "Unexpected case!"
  where
    startBlocked = getElem 1 500 caveMap == sand
    down = safeGet (sandRow + 1) sandCol caveMap
    safeDown = fromJust down
    left = getElem (sandRow + 1) (sandCol - 1) caveMap
    right = getElem (sandRow + 1) (sandCol + 1) caveMap

main :: IO ()
main = do
    input <- readLines (getDataFileName "2022/14_regolith_reservoir/input.txt")
    let rockInput = map processLine input
    let emptyMap = matrix 500 1000 (const air)
    let mapWithStart = setElem '+' (swap sandStart) emptyMap -- TODO: why is col/row inversed for set?
    let mapWithRocks = foldl addRockPath mapWithStart rockInput
    let (mapWithSand, steps) = simulateSand (mapWithRocks, 0)
    print $ submatrix 1 20 490 510 mapWithSand -- useful sub-section for test input
    print steps -- expected output: 805
    let lowestRock = foldl max 0 $ map (foldl max 0 . map snd) rockInput
    let mapWithFloor = addRockPath mapWithRocks [(1, lowestRock + 2), (1000, lowestRock + 2)]
    let (mapWithSand2, steps2) = simulateSand (mapWithFloor, 0)
    print $ submatrix 1 20 490 510 mapWithSand2 -- useful sub-section for test input
    print steps2
