import Input (readLines, readVentLines, removeDiagonals)
import Map (Map, initializeMap, addLine, filterBy)
import Data.List (intercalate)

-- just for debugging purposes
printMatrix :: [[Int]] -> IO ()
printMatrix matrix = putStrLn $ unlines (map unwords stringMatrix)
                        where stringMatrix = map (map show) matrix

countHazards :: Map -> Int
countHazards input = sum (map length filteredInput)
                        where filteredInput = filterBy input (1 <) 

main :: IO ()
main = do   lines <- readLines "input.txt"
            let ventLines = removeDiagonals $ readVentLines lines []
            let ventMap = foldl addLine initializeMap ventLines
            print $ countHazards ventMap
            