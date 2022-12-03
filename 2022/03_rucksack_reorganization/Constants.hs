module Constants
    ( priority
    ) where

import Data.List (elemIndex)
import Data.Maybe (fromJust, isJust, isNothing)

letters :: [Char]
letters = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"

priority :: Char -> Int
priority char
    | isJust index = fromJust index + 1
    | isNothing index = error $ "priority not found for: " ++ [char]
    | otherwise = error $ "unexpected case in `priority`: " ++ [char]
  where
    index = elemIndex char letters
