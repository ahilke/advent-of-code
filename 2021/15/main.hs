import Input (readLines)
import Display (isUnambiguous, allPossibleCombinations, checkForMatches)

main :: IO ()
main = do   input <- readLines "input.txt"
            let sumOfUnambiguous = sum $ map (countTrue . unambiguousInLine) digitOutputs
                digitOutputs = map snd input
                unambiguousInLine :: [[Char]] -> [Bool]
                unambiguousInLine outputLine = map (isUnambiguous allPossibleCombinations) outputLine
                countTrue :: [Bool] -> Int
                countTrue input = length $ filter id input

            print sumOfUnambiguous
