import Input (readLines)
import Map (getNeighbors)
import Data.Matrix (getElem, Matrix (ncols, nrows), fromList)

main :: IO ()
main = do   matrix <- readLines "input.txt"
            let width = nrows matrix
                height = ncols matrix
                indices = [(row, col) | row <- [1..width], col <- [1..height]]
                neighborMatrix = fromList width height $ map (getNeighbors matrix) indices
                sumOfLowest = foldr adder 0 indices
                    where adder (row, col) acc
                            | value < minimum neighbors = acc + value + 1
                            | otherwise = acc
                            where 
                                value = getElem row col matrix
                                neighbors = getElem row col neighborMatrix

            print sumOfLowest
