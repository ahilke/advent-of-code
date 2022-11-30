
module Bingo(Board, Field, updateBoard, isWinningBoard, scoreBoard) where

import Data.List (transpose)

type Board = [[Field]]
type Field = (Int, Bool)

isWinningBoard :: Board -> Bool
isWinningBoard board = any isWinningRow board || any isWinningRow (transpose board)

isWinningRow :: [Field] -> Bool
isWinningRow = all snd

updateBoard :: Int -> Board -> Board
updateBoard number = map updateRow
                        where updateRow = map (updateField number)

updateField :: Int -> Field -> Field
updateField number field 
    | number == fst field = (number, True)
    | otherwise = field

scoreBoard :: Int -> Board -> Int
scoreBoard number board = number * unmarkedNumbers
                            where   
                                unmarkedNumbers = sum rows
                                rows = map (foldr addField 0) board

addField :: Field -> Int -> Int
addField field acc
    | snd field = acc
    | otherwise = acc + fst field

