module Display (allPossibleCombinations, checkForMatches, isUnambiguous) where

import Data.List (subsequences)
import Data.List.NonEmpty (xor, nonEmpty)
import Data.Maybe (fromJust)

segments :: [Char]
segments = "abcdefg"

litSegments :: [Int]
litSegments = [6, 2, 5, 5, 4, 5, 6, 3, 7, 6]

-- for each digit (i.e. index), contains all possible patterns
allPossibleCombinations :: [[[Char]]]
allPossibleCombinations = map (getCombinations segments) litSegments

getCombinations :: [a] -> Int -> [[a]]
getCombinations list k = filter (( == k) . length) (subsequences list)

-- for which digit a given pattern is possible
checkForMatches :: [Char] -> [[[Char]]] -> [Bool]
checkForMatches digitOutput = map (elem digitOutput)

isUnambiguous :: [[[Char]]] -> [Char] -> Bool
isUnambiguous possibleCombinations digitOutput 
    = (== 1) $ length $ filter id (checkForMatches digitOutput possibleCombinations)
