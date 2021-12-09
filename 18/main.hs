import Input (readLines)
import Map (getNeighbors, addToBasin)
import Data.Matrix (getElem, Matrix (ncols, nrows), fromList)
import Data.List (sort, nub)

main :: IO ()
main = do   matrix <- readLines "input.txt"
            let width = nrows matrix
                height = ncols matrix
                indices = [(row, col) | row <- [1..width], col <- [1..height]]
                neighborMatrix = fromList width height $ map (getNeighbors matrix) indices
                lowestPoints = foldr adder [] indices
                    where adder (row, col) acc
                            | value < minimum neighbors = (row, col):acc
                            | otherwise = acc
                            where 
                                value = getElem row col matrix
                                neighbors = getElem row col neighborMatrix
                basins = map (nub . mapper matrix []) lowestPoints
                    where mapper matrix basins point = addToBasin matrix point basins   
                basinSizes = reverse $ sort $ map length basins
                (x:y:z:otherBasins) = basinSizes

            print $ x * y * z
