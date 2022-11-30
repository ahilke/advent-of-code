import Data.List (find)
import Data.List.Split (splitOn)
import Input (readLines, readBoards)
import Bingo (Board, isWinningBoard, updateBoard, scoreBoard)

game :: [Int] -> [Board] -> Int
game numbers boards 
    = case winningBoard of Nothing -> game otherNumbers updatedBoards
                           Just winningBoard -> scoreBoard nextNumber winningBoard
                        where 
                            (nextNumber:otherNumbers) = numbers
                            winningBoard = find isWinningBoard updatedBoards
                            updatedBoards = map (updateBoard nextNumber) boards


main :: IO ()
main = do   lines <- readLines "input.txt"
            let (numberLine:boardLines) = lines
                numbers :: [Int]
                numbers = map read $ splitOn "," numberLine
                boards = readBoards boardLines []
            print $ game numbers boards
