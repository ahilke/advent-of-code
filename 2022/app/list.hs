module List
    ( replace
    ) where

-- this is equivalent to `(element i .~ value) list` when using Control.Lens
replace :: a -> Int -> [a] -> [a]
replace value i list = take i list ++ [value] ++ drop (i + 1) list
