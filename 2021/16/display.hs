module Display (findCombinations, allPossibleCombinations, decodeOutput, solve, fromDigits) where

import Data.List (subsequences, find, elemIndex, intersect, (\\), union)
import Data.Maybe (fromJust)
import Debug.Trace
import Control.Lens (set, element)

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

fromDigits :: [Int] -> Int
fromDigits = foldl addDigit 0
    where 
        addDigit :: Int -> Int -> Int
        addDigit acc digit = 10 * acc + digit

decodeOutput :: [[Char]] -> [[Char]] -> [Int]
decodeOutput output matchedCombinations = map (fromJust . flip elemIndex matchedCombinations) output

-- 1. find an input that is unambiguous
-- 2. set that combination as only possible combinations for the given number
-- repeat until every number is unambiguous
findCombinations :: [[Char]] -> [[[Char]]] -> [[Char]]
-- findCombinations input possibleCombinations | trace (show (input, possibleCombinations)) False = undefined 
findCombinations input possibleCombinations
    -- every number is identified, unwrap and return (FIXME: check for empty input list instead?)
    | all ((== 1) . length) possibleCombinations = map head possibleCombinations
    | otherwise = findCombinations newInput newCombinations
    where
        nextDigit = fromJust $ find (isUnambiguous possibleCombinations) input
        newCombinations = removeInvalidCombinations nextDigit possibleCombinations
        newInput :: [[Char]]
        newInput = filter (/= nextDigit) input

removeInvalidCombinations :: [Char] -> [[[Char]]] -> [[[Char]]]
removeInvalidCombinations digitToMatch possibleCombinations
    | digitToMatch `elem` combinations = [digitToMatch]:otherCombinations
    | otherwise = combinations : removeInvalidCombinations digitToMatch otherCombinations
    where
        combinations:otherCombinations = possibleCombinations
        

solve :: [[Char]] -> [[[Char]]]
solve input = filterHasNotABDEG
    where
        one = fromJust $ find ((== 2) . length) input
        solveOne = set (element 1) [one] allPossibleCombinations 

        four = fromJust $ find ((== 4) . length) input
        solveFour = set (element 4) [four] solveOne 

        seven = fromJust $ find ((== 3) . length) input
        solveSeven = set (element 7) [seven] solveFour 

        eight = fromJust $ find ((== 7) . length) input
        solveEight = set (element 8) [eight] solveSeven 

        filterOne = mustHaveAll solveEight one [0, 3, 9]
        filterFour = mustHaveAll filterOne four [9]
        filterSeven = mustHaveAll filterFour seven [0, 3, 9]

        cf = one `intersect` four
        filterRightLine = mustNotHaveAll filterSeven cf [2, 5, 6]

        bd = four \\ one
        filterHasBD = mustHaveAll filterRightLine bd [5, 6, 9]
        filterHasNotBD = mustNotHaveAll filterHasBD bd [0, 2, 3]

        bdeg = eight \\ seven
        filterHasBDEG = mustHaveAll filterHasNotBD bdeg [6]
        filterHasNotBDEG = mustNotHaveAll filterHasBDEG bdeg [0, 2, 3, 5, 9]

        filterHasNotOne = mustNotHaveAll filterHasNotBDEG one [2, 5, 6]
        filterHasNotFour = mustNotHaveAll filterHasNotOne four [0, 2, 3, 5, 6]
        filterHasNotSeven = mustNotHaveAll filterHasNotFour seven [2, 5, 6]

        abd = (seven `union` four) \\ one
        filterHasABD = mustHaveAll filterHasNotSeven abd [5, 6, 9]
        filterHasNotABD = mustNotHaveAll filterHasABD abd [0, 2, 3]

        abcdf = seven `union` four
        filterHasABCDF = mustHaveAll filterHasNotABD abcdf [9]
        filterHasNotABCDF = mustNotHaveAll filterHasABCDF abcdf [0, 2, 3, 5, 6]

        aeg = eight \\ four
        filterHasAEG = mustHaveAll filterHasNotABCDF aeg [0, 2, 6]
        filterHasNotAEG = mustNotHaveAll filterHasAEG aeg [3, 5, 9]

        acefg = aeg `union` seven
        filterHasAECGG = mustHaveAll filterHasNotAEG acefg [0]
        filterHasNotAECGG = mustNotHaveAll filterHasAECGG acefg [2, 3, 5, 6, 9]

        eg = acefg \\ seven
        filterHasEG = mustHaveAll filterHasNotAECGG eg [0, 2, 6]
        filterHasNotEG = mustNotHaveAll filterHasEG eg [3, 5, 9]

        abdeg = eight \\ one
        filterHasABDEG = mustHaveAll filterHasNotEG abdeg [6]
        filterHasNotABDEG = mustNotHaveAll filterHasABDEG abdeg [0, 2, 3, 5, 9]

-- filters out combinations for given digits where not all segments of the search string are contained
mustHaveAll :: [[[Char]]] -> [Char] -> [Int] -> [[[Char]]]
mustHaveAll combinations search [] = combinations
mustHaveAll combinations search (digit:digits) = mustHaveAll newCombinations search digits
    where 
        newCombinations = set (element digit) filtered combinations
        listToFilter = combinations !! digit
        filtered = filter ((==search) . intersect search) listToFilter

-- filters out combinations for given digits where all segments of the search string are contained
mustNotHaveAll :: [[[Char]]] -> [Char] -> [Int] -> [[[Char]]]
mustNotHaveAll combinations search [] = combinations
mustNotHaveAll combinations search (digit:digits) = mustNotHaveAll newCombinations search digits
    where 
        newCombinations = set (element digit) filtered combinations
        listToFilter = combinations !! digit
        filtered = filter ((/=search) . intersect search) listToFilter
