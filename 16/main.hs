import Input (readLines)
import Display (findCombinations, allPossibleCombinations, decodeOutput, solve, fromDigits)
import Data.List (intercalate, sort)

exampleInput = map sort $ words "acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab"
exampleDigits = map sort $ words "cdfeb fcadb cdfeb cdbaf"

solveOneLine :: [[Char]] -> [[Char]] -> Int
solveOneLine input digits = fromDigits decodedOutput
    where
        decodedOutput = decodeOutput digits matchedCombinations
        matchedCombinations = findCombinations input narrowedCombinations
        narrowedCombinations = solve input

solver :: [([[Char]], [[Char]])] -> Int
solver lines = sum solvedLines
    where
        solvedLines = map lineSolver lines
        lineSolver (input, digits) = solveOneLine input digits

main :: IO ()
main = do   input <- readLines "input.txt"

            let test = solveOneLine exampleInput exampleDigits
            print test
           
            let result = solver input
            print result
