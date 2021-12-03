import Data.List
import Data.Char (digitToInt, intToDigit)

readLines :: FilePath -> IO [String]
readLines path = do contents <- readFile path
                    return (lines contents)

toIntMatrix :: [[Char]] -> [[Int]]
toIntMatrix = map (map digitToInt) 

countOnesPerRow :: [[Int]] -> [Int]
countOnesPerRow = map sum

digitsToNumber :: [Int] -> Int
digitsToNumber digits = let string = map intToDigit digits
                        in read string

toGamma :: Int -> [Int] -> Int
toGamma length ones = let digits = map (mostCommonBit length) ones
                      in binToDec (map intToDigit digits)
                      
toEpsilon :: Int -> [Int] -> Int
toEpsilon length ones = let digits = map (invert . mostCommonBit length) ones
                        in binToDec (map intToDigit digits)

invert :: Int -> Int
invert 0 = 1
invert 1 = 0
invert _ = error "not a bit!"

binToDec :: String -> Int
binToDec = foldl addDigit 0 

addDigit :: Int -> Char -> Int
addDigit acc digit = acc * 2 + digitToInt digit

mostCommonBit :: Int -> Int -> Int 
mostCommonBit linesLength ones
    | fOnes > fLinesLength / 2 = 1
    | fOnes < fLinesLength / 2 = 0
    | otherwise = error "exactly half!"
    where 
        fOnes = fromIntegral ones
        fLinesLength = fromIntegral linesLength

main :: IO ()
main = do lines <- readLines "input.txt"
          let linesLength = length lines
          let transposedDigits = transpose $ toIntMatrix lines
          let ones = countOnesPerRow transposedDigits
          let gamma = toGamma linesLength ones
          let epsilon = toEpsilon linesLength ones
          print $ gamma * epsilon          
