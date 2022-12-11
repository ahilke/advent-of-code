module Main where

import Paths_advent_of_code (getDataFileName)
import Read (readLines)

import Debug.Trace (trace)

data State =
    State
        { x :: Int
        , clockCycle :: Int
        , signalStrength :: Int
        }
    deriving (Show)

isSignalStrengthCycle :: Int -> Bool
isSignalStrengthCycle value = mod value 40 == 20

incrementCycle :: State -> State
incrementCycle state =
    if isSignalStrengthCycle (clockCycle state)
        then state
                 { clockCycle = clockCycle state + 1
                 , signalStrength = signalStrength state + (clockCycle state * x state)
                 }
        else state {clockCycle = clockCycle state + 1}

noop :: State -> State
noop = incrementCycle

-- Note: Cycle needs to be incremented before changing `x`
addX :: State -> Int -> State
addX state value = (iterate incrementCycle state !! 2) {x = x state + value}

processLine :: State -> String -> State
processLine state input
    | head inputWords == "noop" = noop state
    | head inputWords == "addx" = addX state (read $ inputWords !! 1)
    | otherwise = error $ "error processing line: " ++ input
  where
    inputWords = trace (show state ++ " -> " ++ input) words input

main :: IO ()
main = do
    input <- readLines (getDataFileName "2022/10_cathode_ray_tube/input.txt")
    let endState = foldl processLine State {x = 1, clockCycle = 1, signalStrength = 0} input
    print endState
