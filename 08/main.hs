import Data.List (find)
import Data.List.Split (splitOn)
import Input (readLines, readBoards)
import Bingo (Board, isWinningBoard, updateBoard, scoreBoard)

game :: [Int] -> [Board] -> Int
game numbers boards 
    | length boards == 1 && isWinningBoard (head updatedBoards) =  scoreBoard nextNumber (head updatedBoards)
    | otherwise = game otherNumbers losingBoards
        where 
            (nextNumber:otherNumbers) = numbers
            losingBoards = filter (not . isWinningBoard) updatedBoards
            updatedBoards = map (updateBoard nextNumber) boards


main :: IO ()
main = do   lines <- readLines "input.txt"
            let (numberLine:boardLines) = lines
                numbers :: [Int]
                numbers = map read $ splitOn "," numberLine
                boards = readBoards boardLines []
            print $ game numbers boards
