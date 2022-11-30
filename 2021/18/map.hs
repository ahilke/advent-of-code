module Map (getNeighbors, addToBasin) where

import Data.Matrix (Matrix, safeGet, getElem, setElem)
import Data.Maybe (fromJust, isNothing, isJust)
import Debug.Trace
import Data.List ((\\))

getNeighbors :: Matrix Int -> (Int, Int) -> [Int]
getNeighbors matrix (row, column)
    = addItem matrix (row - 1, column - 1)
        $ addItem matrix (row - 1, column)
        $ addItem matrix (row - 1, column + 1)
        $ addItem matrix (row, column - 1)
        $ addItem matrix (row, column + 1)
        $ addItem matrix (row + 1, column - 1)
        $ addItem matrix (row + 1, column)
        $ addItem matrix (row + 1, column + 1)
        []

addItem :: Matrix Int -> (Int, Int) -> [Int] -> [Int]
addItem matrix (row, column) items
    | isNothing maybeItem = items
    | isJust maybeItem = fromJust maybeItem : items
    | otherwise = undefined
    where maybeItem = safeGet row column matrix

-- TODO: Better recursion, have a list of points to visit
addToBasin :: Matrix Int -> [(Int, Int)] -> [(Int, Int)] -> [(Int, Int)] -> [(Int, Int)]
-- addToBasin matrix basin visited toVisit | trace (show [basin, visited, toVisit]) False = undefined
addToBasin matrix basin visited toVisit
    | null toVisit = basin
    | isNothing item = addToBasin matrix basin visited nextPoints
    | item == Just 9 = addToBasin matrix basin visited nextPoints
    | otherwise = addToBasin matrix ((row, column):basin) ((row, column):visited) (notVisitedNeighbors ++ nextPoints)
    where
        point:nextPoints = toVisit
        (row, column) = point
        item = safeGet row column matrix
        neighbors = [(row - 1, column), (row + 1, column), (row, column - 1), (row, column + 1)]
        notVisitedNeighbors = (neighbors \\ visited) \\ toVisit
        