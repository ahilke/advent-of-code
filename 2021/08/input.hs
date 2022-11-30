module Input(readLines, readBoards) where

import Bingo (Board)

readLines :: FilePath -> IO [String]
readLines path = do contents <- readFile path
                    return (lines contents)

readBoards :: [String] -> [Board] -> [Board]
readBoards lines boards = foldl readNext boards lines

readNext :: [Board] -> String -> [Board]
readNext boards line
    | line == "" = []:boards
    | otherwise = (firstBoard ++ [row]):otherBoards
                    where
                        (firstBoard:otherBoards) = boards
                        row = zip numbers (repeat False)
                        numbers = map read $ words line
