module Map (getNeighbors) where

import Data.Matrix (Matrix, safeGet)
import Data.Maybe (fromJust, isNothing, isJust)

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
    | otherwise = error "unexpected case"
    where maybeItem = safeGet row column matrix
