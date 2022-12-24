module Main where

import List (findAllIn2D, findIn2D)
import Paths_advent_of_code (getDataFileName)
import Read (readLines)

import Data.Char (ord)
import Data.Matrix (Matrix, getElem, safeGet)
import Data.Matrix as Matrix (fromLists)
import Data.Maybe (catMaybes, isJust)
import Data.Set (Set, difference, empty, insert, member)
import Data.Set as Set (fromList, toList)

data Node =
    Node
        { content :: Char
        , pos :: (Int, Int)
        }

-- add 1 as Matrix is 1-indexed while lists are 0-indexed
-- swap row and col order to match expected order
findStart :: [[Char]] -> (Int, Int)
findStart heightMap = (startCol + 1, startRow + 1)
  where
    (startRow, startCol) = findIn2D heightMap 'S'

-- add 1 as Matrix is 1-indexed while lists are 0-indexed
-- swap row and col order to match expected order
findAllStartPoints :: [[Char]] -> [(Int, Int)]
findAllStartPoints heightMap = start : oneIndexed
  where
    as = findAllIn2D heightMap 'a'
    start = findStart heightMap
    oneIndexed = [(col + 1, row + 1) | (row, col) <- as] -- add 1 as Matrix is 1-indexed while lists are 0-indexed

getMaybePoint :: Matrix a -> (Int, Int) -> Maybe (Int, Int)
getMaybePoint heightMap (col, row) =
    if isJust maybeElement
        then Just (col, row)
        else Nothing
  where
    maybeElement = safeGet row col heightMap

getNeighbors :: Matrix Char -> (Int, Int) -> [(Int, Int)]
getNeighbors heightMap (col, row) = catMaybes maybeNeighbors
  where
    maybeNeighbors =
        [ getMaybePoint heightMap (col, row - 1)
        , getMaybePoint heightMap (col, row + 1)
        , getMaybePoint heightMap (col - 1, row)
        , getMaybePoint heightMap (col + 1, row)
        ]

getHeight :: Char -> Int
getHeight 'S' = 1
getHeight 'E' = 26
getHeight char = ord char - ord 'a' + 1

isTraversable :: Char -> Char -> Bool
isTraversable start goal = getHeight goal - getHeight start <= 1

getTraversableNeighbors :: Matrix Char -> (Int, Int) -> [(Int, Int)]
getTraversableNeighbors heightMap (col, row) =
    [ (nCol, nRow)
    | (nCol, nRow) <- getNeighbors heightMap (col, row)
    , isTraversable (getElem row col heightMap) (getElem nRow nCol heightMap)
    ]

breadthFirstSearch :: Matrix Char -> Int -> (Int, Int) -> Set (Int, Int) -> [(Int, Int)] -> Int
breadthFirstSearch heightMap steps endPoint visitedPoints currentPoints
    | member endPoint visitedPoints = steps - 1
    | null currentPoints = error "no more points to visit"
    | otherwise = breadthFirstSearch heightMap (steps + 1) endPoint newVisitedPoints nextPoints
  where
    newVisitedPoints = foldl (flip insert) visitedPoints currentPoints
    allTraversableNeighbors = Set.fromList $ concatMap (getTraversableNeighbors heightMap) currentPoints
    unvisitedTraversableNeighbors = difference allTraversableNeighbors visitedPoints
    -- nextPoints = trace (printf "steps: %d, current: %s" steps (show currentPoints)) toList unvisitedTraversableNeighbors
    nextPoints = toList unvisitedTraversableNeighbors

main :: IO ()
main = do
    input <- readLines (getDataFileName "2022/12_hill_climbing/input.txt")
    let heightMap = Matrix.fromLists input
    let steps = 0
    let visitedPoints = empty
    let (endRow, endCol) = findIn2D input 'E'
    let currentPoints = [findStart input]
    print $ breadthFirstSearch heightMap steps (endCol + 1, endRow + 1) visitedPoints currentPoints
    let startPoints = findAllStartPoints input
    print $ breadthFirstSearch heightMap steps (endCol + 1, endRow + 1) visitedPoints startPoints
