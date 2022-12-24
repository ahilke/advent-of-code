-- TODO: this is not type checked by vscode
module List
    ( replace
    , findIn2D
    , findAllIn2D
    ) where

import Data.List (elemIndex, elemIndices, find, findIndex, findIndices)
import Data.Maybe (fromJust)

-- this is equivalent to `(element i .~ value) list` when using Control.Lens
replace :: a -> Int -> [a] -> [a]
replace value i list = take i list ++ [value] ++ drop (i + 1) list

findIn2D :: Eq a => [[a]] -> a -> (Int, Int)
findIn2D list2D x = (rowIndex, colIndex)
  where
    rowIndex = fromJust $ findIndex (elem x) list2D
    row = fromJust $ find (elem x) list2D
    colIndex = fromJust $ elemIndex x row

findAllIn2D :: Eq a => [[a]] -> a -> [(Int, Int)]
findAllIn2D list2D x = result
  where
    rowIndices = findIndices (elem x) list2D
    colIndicesList = map matchingColumnsInRow rowIndices
      where
        matchingColumnsInRow rowIndex = elemIndices x row
          where
            row = list2D !! rowIndex
    result = concat $ zipWith zipRowWithCol rowIndices colIndicesList
      where
        zipRowWithCol rowIndex colIndices = [(rowIndex, col) | col <- colIndices]
