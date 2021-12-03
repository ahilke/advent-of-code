import Data.List (transpose)
import Data.Char (digitToInt, intToDigit)
import Bin (binToDec)
import Diagnostic (toGamma, toEpsilon)

readLines :: FilePath -> IO [String]
readLines path = do contents <- readFile path
                    return (lines contents)

toIntMatrix :: [[Char]] -> [[Int]]
toIntMatrix = map (map digitToInt) 

countOnesPerRow :: [[Int]] -> [Int]
countOnesPerRow = map sum

main :: IO ()
main = do lines <- readLines "input.txt"
          let linesLength = length lines
          let transposedDigits = transpose $ toIntMatrix lines
          let ones = countOnesPerRow transposedDigits
          let gamma = toGamma linesLength ones
          let epsilon = toEpsilon linesLength ones
          print $ gamma * epsilon
