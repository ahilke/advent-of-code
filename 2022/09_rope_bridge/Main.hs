module Main where

import Paths_advent_of_code (getDataFileName)
import Read (readLines)

import Data.Set (Set, empty, insert, size)
import Text.Printf (printf)

data State =
    State
        { visited :: Set (Int, Int)
        , headPos :: (Int, Int)
        , tailPos :: (Int, Int)
        }
    deriving (Show)

moveHead :: (Int, Int) -> Char -> (Int, Int)
moveHead (x, y) 'R' = (x + 1, y)
moveHead (x, y) 'L' = (x - 1, y)
moveHead (x, y) 'U' = (x, y + 1)
moveHead (x, y) 'D' = (x, y - 1)
moveHead _ _ = error "error on moveHead"

moveTail :: (Int, Int) -> (Int, Int) -> (Int, Int)
moveTail (xHead, yHead) (xTail, yTail)
    | abs (xTail - xHead) <= 1 && abs (yHead - yTail) <= 1 = (xTail, yTail)
    | xTail == xHead && yTail > yHead + 1 = (xTail, yTail - 1)
    | xTail == xHead && yTail < yHead - 1 = (xTail, yTail + 1)
    | yTail == yHead && xTail > xHead + 1 = (xTail - 1, yTail)
    | yTail == yHead && xTail < xHead - 1 = (xTail + 1, yTail)
    | xTail < xHead && yTail < yHead = (xTail + 1, yTail + 1)
    | xTail < xHead && yTail > yHead = (xTail + 1, yTail - 1)
    | xTail > xHead && yTail < yHead = (xTail - 1, yTail + 1)
    | xTail > xHead && yTail > yHead = (xTail - 1, yTail - 1)
    | otherwise = error $ printf "error moving tail %s to head %s" (show (xHead, yHead)) (show (xTail, yTail))

processDirection :: State -> Char -> State
processDirection input direction = newState
  where
    newHeadPos = moveHead (headPos input) direction
    newTailPos = moveTail newHeadPos (tailPos input)
    newVisited = insert newTailPos (visited input)
    newState = State {visited = newVisited, headPos = newHeadPos, tailPos = newTailPos}

main :: IO ()
main = do
    input <- readLines (getDataFileName "2022/09_rope_bridge/input.txt")
    let directionPairs = map words input
    let directions = concat $ concatMap replicateDirections directionPairs
          where
            replicateDirections [a, b] = replicate (read b) a
            replicateDirections _ = error "error on replicateDirections"
    let endState = foldl processDirection State {visited = empty, headPos = (0, 0), tailPos = (0, 0)} directions
    print $ size $ visited endState
