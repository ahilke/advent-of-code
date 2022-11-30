import Data.List (transpose, intercalate)
import Data.Char (digitToInt, intToDigit)
import Bin (binToDec)

readLines :: FilePath -> IO [String]
readLines path = do contents <- readFile path
                    return (lines contents)

toIntMatrix :: [[Char]] -> [[Int]]
toIntMatrix = map (map digitToInt) 

countOnesPerRow :: [[Int]] -> [Int]
countOnesPerRow = map sum

filterList :: ([[Int]] -> Int -> Int -> Int) -> [[Int]] -> Int -> Int -> [Int]
filterList filterFunction input position tieValue 
    | length input == 1 = head input
    | otherwise = filterList filterFunction newInput (position + 1) tieValue
                    where newInput = filterBy filterFunction input position tieValue

filterBy :: ([[Int]] -> Int -> Int -> Int) -> [[Int]] -> Int -> Int -> [[Int]]
filterBy filterFunction input position tieValue = filter (hasValueAt filterValue position) input  
                                                    where filterValue = filterFunction input position tieValue

mostCommonBitAt :: [[Int]] -> Int -> Int -> Int
mostCommonBitAt input position = mostCommonBit inputLength onesAt
                                 where inputLength = length input
                                       onesAt = countOnesPerRow (transpose input)!!position

hasValueAt :: Eq a => a -> Int -> [a] -> Bool 
hasValueAt value pos list = list!!pos == value

mostCommonBit :: Int -> Int -> Int -> Int 
mostCommonBit linesLength ones tieValue
    | fOnes > fLinesLength / 2 = 1
    | fOnes < fLinesLength / 2 = 0
    | otherwise = tieValue
    where 
        fOnes = fromIntegral ones
        fLinesLength = fromIntegral linesLength

leastCommonBitAt :: [[Int]] -> Int -> Int -> Int
leastCommonBitAt input position tieValue = invert $ mostCommonBitAt input position tieValue

invert :: Int -> Int
invert 0 = 1
invert 1 = 0
invert _ = error "not a bit!"

toValue :: [Int] -> Int
toValue rating = binToDec (map intToDigit rating)

main :: IO ()
main = do lines <- readLines "input.txt"
          let digits = toIntMatrix lines

          let oxygenRating = filterList mostCommonBitAt digits 0 1
          let scrubberRating = filterList leastCommonBitAt digits 0 1

          print $ toValue oxygenRating * toValue scrubberRating
