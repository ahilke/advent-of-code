module Main where

import Paths_advent_of_code (getDataFileName)
import Read (readLines)

import Data.List (transpose)
import Data.List.Split (splitOn)
import Data.String.Utils (strip)
import Debug.Trace (trace)

takeEvery :: Int -> [a] -> [a]
takeEvery n xs =
    case drop (n - 1) xs of
        y:ys -> y : takeEvery n ys
        [] -> []

splitTwo :: Eq a => [a] -> [a] -> ([a], [a])
splitTwo separator input =
    case splitOn separator input of
        [a, b] -> (a, b)
        _ -> error "not exactly one blank line in input"

-- parsed "move a from b to c" to (a, b, c)
parseMoveInstruction :: String -> (Int, Int, Int)
parseMoveInstruction input = (read a, read b, read c)
  where
    fromA = drop (length "move ") input -- "a from b to c"
    (a, fromB) = splitTwo " from " fromA -- "b to c"
    (b, c) = splitTwo " to " fromB

-- this is equivalent to `(element i .~ value) list` when using Control.Lens
replace :: a -> Int -> [a] -> [a]
replace value i list = take i list ++ [value] ++ drop (i + 1) list

processMoveInstruction9000 :: [String] -> (Int, Int, Int) -> [String]
processMoveInstruction9000 stacks (count, oneIndexedStart, oneIndexedTarget) = trace (show newStacks) newStacks
  where
    start = oneIndexedStart - 1
    target = oneIndexedTarget - 1
    startStack = stacks !! start
    targetStack = stacks !! target
    (movedCrates, newStartStack) = splitAt count startStack
    newTargetStack = reverse movedCrates ++ targetStack
    newStacks = (replace newStartStack start . replace newTargetStack target) stacks

-- the only difference is that it does not reverse `movedCrates`
-- I could not be bothered to reduce duplication
processMoveInstruction9001 :: [String] -> (Int, Int, Int) -> [String]
processMoveInstruction9001 stacks (count, oneIndexedStart, oneIndexedTarget) = trace (show newStacks) newStacks
  where
    start = oneIndexedStart - 1
    target = oneIndexedTarget - 1
    startStack = stacks !! start
    targetStack = stacks !! target
    (movedCrates, newStartStack) = splitAt count startStack
    newTargetStack = movedCrates ++ targetStack
    newStacks = (replace newStartStack start . replace newTargetStack target) stacks

main :: IO ()
main = do
    input <- readLines (getDataFileName "2022/05_supply_stacks/input.txt")
    let (rawStackInput, rawMoveInstructions) = splitTwo [""] input
    let stackInput = init rawStackInput -- drop last line containing stack numbers
    let padding = "  "
    let stacksTransposed = map toStack stackInput
          where
            toStack line = takeEvery 4 (padding ++ line)
    let stacksUntrimmed = transpose stacksTransposed -- nth entry is the nth stack from top to bottom
    let stacks = map strip stacksUntrimmed
    let moveInstructions = map parseMoveInstruction rawMoveInstructions
    let resultStacks9000 = foldl processMoveInstruction9000 stacks moveInstructions
    let topCrates9000 = map (take 1) resultStacks9000
    print $ concat topCrates9000
    let resultStacks9001 = foldl processMoveInstruction9001 stacks moveInstructions
    let topCrates9001 = map (take 1) resultStacks9001
    print $ concat topCrates9001
