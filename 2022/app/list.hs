module List
    ( replace
    , findIn2D
    ) where

import Data.List (elemIndex, find, findIndex)
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
