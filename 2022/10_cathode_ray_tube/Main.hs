module Main where

import Paths_advent_of_code (getDataFileName)
import Read (readLines)

import Data.Matrix (Matrix, fromLists, setElem, toLists)
import Debug.Trace (trace)
import Text.Printf

data State =
    State
        { x :: Int
        , clockCycle :: Int
        , signalStrength :: Int
        , screen :: Matrix Char
        }

instance Show State where
    show state =
        printf "cycle: %d, x: %d, signal: %d" (clockCycle state) (x state) (signalStrength state) ++ "\n" ++ showScreen
      where
        showScreen = unlines $ toLists $ screen state

isSignalStrengthCycle :: Int -> Bool
isSignalStrengthCycle value = mod value 40 == 20

processCycle :: State -> State
processCycle = incrementCycle . draw

incrementCycle :: State -> State
incrementCycle state =
    if isSignalStrengthCycle (clockCycle state)
        then state
                 { clockCycle = clockCycle state + 1
                 , signalStrength = signalStrength state + (clockCycle state * x state)
                 }
        else state {clockCycle = clockCycle state + 1}

draw :: State -> State
draw state =
    if spriteVisible
        -- +1 for row/column, as matrices are 1-indexed
        then state {screen = lightPixel (row + 1, column + 1) (screen state)}
        else state
  where
    spriteVisible = abs (column - x state) <= 1
    column = mod (clockCycle state - 1) 40 -- cycle starts at 1, but screen index at 0
    row = div (clockCycle state - 1) 40 -- cycle starts at 1, but screen index at 0

incrementRegister :: State -> Int -> State
incrementRegister state value = state {x = x state + value}

lightPixel :: (Int, Int) -> Matrix Char -> Matrix Char
lightPixel = setElem '#'

noop :: State -> State
noop = processCycle

-- Note: Cycle needs to be incremented before changing `x`
addX :: State -> Int -> State
addX state value = (iterate processCycle state !! 2) {x = x state + value}

processLine :: State -> String -> State
processLine state input
    | head inputWords == "noop" = noop state
    | head inputWords == "addx" = addX state (read $ inputWords !! 1)
    | otherwise = error $ "error processing line: " ++ input
  where
    inputWords = trace (show state ++ " -> " ++ input) words input

-- showState state = 
main :: IO ()
main = do
    input <- readLines (getDataFileName "2022/10_cathode_ray_tube/input.txt")
    let endState =
            foldl
                processLine
                State {x = 1, clockCycle = 1, signalStrength = 0, screen = fromLists $ replicate 6 $ replicate 40 '.'}
                input
    print endState
