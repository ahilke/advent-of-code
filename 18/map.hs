module Map (getNeighbors, addToBasin) where

import Data.Matrix (Matrix, safeGet, getElem, setElem)
import Data.Maybe (fromJust, isNothing, isJust)
import Debug.Trace

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
addToBasin :: Matrix Int -> (Int, Int) -> [(Int, Int)] -> [(Int, Int)]
-- addToBasin matrix (row, column) basin | trace (show (row, column) ++ show basin) False = undefined
addToBasin matrix (row, column) basin
    | isNothing item = basin
    | item == Just 9 = basin
    | otherwise = addToBasin newMatrix (row - 1, column) 
                $ addToBasin newMatrix (row + 1, column) 
                $ addToBasin newMatrix (row, column - 1) 
                $ addToBasin newMatrix (row, column + 1)  
                ((row, column):basin)
    where
        item = safeGet row column matrix
        newMatrix = setElem 9 (row, column) matrix -- to not add the same element twice