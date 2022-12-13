module Main where

import List (replace)
import Paths_advent_of_code (getDataFileName)
import Read (readLines)

import Data.Ix (range)
import Data.List (sort)
import Data.List.Split (splitOn)
import Debug.Trace (trace)
import Text.Printf (printf)

data Monkey =
    Monkey
        { items :: [Int]
        , inspectedItems :: Int
        , operation :: Int -> Int
        , test :: Int -> Int
        }

instance Show Monkey where
    show monkey = printf "inspected items: %d" (inspectedItems monkey)

instance Eq Monkey where
    a == b = inspectedItems a == inspectedItems b

instance Ord Monkey where
    compare a b = compare (inspectedItems a) (inspectedItems b)

readMonkey :: [String] -> Monkey
readMonkey input = Monkey {items = initItems, inspectedItems = 0, operation = initOperation, test = initTest}
  where
    itemLine = input !! 1
    itemString = splitOn ": " itemLine !! 1 -- e.g. "79, 98"
    initItems = map read (splitOn ", " itemString) :: [Int]
    operationLine = input !! 2
    operationString = splitOn "= old " operationLine !! 1 -- e.g. "* 19"
    operationItems = splitOn " " operationString
    initOperation = readOperation operationItems
    divisor = read $ splitOn "divisible by " (input !! 3) !! 1 :: Int
    trueMonkey = read $ splitOn "monkey " (input !! 4) !! 1 :: Int
    falseMonkey = read $ splitOn "monkey " (input !! 5) !! 1 :: Int
    initTest = testOperation divisor trueMonkey falseMonkey

square :: Int -> Int
square x = x ^ (2 :: Int)

readOperation :: [String] -> Int -> Int
readOperation [operator, value]
    | operator == "+" = (+) (read value)
    | operator == "*" && value == "old" = square
    | operator == "*" = (*) (read value)
    | otherwise = error "error in `readOperation`"
readOperation _ = error "error in `readOperation`"

testOperation :: Int -> Int -> Int -> Int -> Int
testOperation divisor trueMonkey falseMonkey value =
    if mod value divisor == 0
        then trueMonkey
        else falseMonkey

updateWorry :: Monkey -> Int -> Int
updateWorry monkey value = div (operation monkey value) 3

processItem :: Int -> [Monkey] -> Int -> [Monkey]
processItem monkeyIndex monkeys worry =
    trace
        (printf "worry: %d -> %d, monkey: %d -> %d" worry newWorry monkeyIndex newMonkeyIndex)
        replace
        newMonkeyWithItem
        newMonkeyIndex
        (replace oldMonkeyWithoutItem monkeyIndex monkeys)
  where
    oldMonkey = monkeys !! monkeyIndex
    newWorry = updateWorry oldMonkey worry
    newMonkeyIndex = test oldMonkey newWorry
    newMonkey = monkeys !! newMonkeyIndex
    oldMonkeyWithoutItem = oldMonkey {items = tail (items oldMonkey), inspectedItems = inspectedItems oldMonkey + 1}
    newMonkeyWithItem = newMonkey {items = items newMonkey ++ [newWorry]}

processMonkey :: [Monkey] -> Int -> [Monkey]
processMonkey monkeys monkeyIndex = foldl (processItem monkeyIndex) monkeys (items monkey)
  where
    monkey = monkeys !! monkeyIndex

playRound :: [Monkey] -> [Monkey]
playRound monkeys = foldl processMonkey monkeys (range (0, length monkeys - 1))

main :: IO ()
main = do
    input <- readLines (getDataFileName "2022/11_monkey_in_the_middle/input.txt")
    let monkeyInputs = splitOn [""] input
    let monkeys = map readMonkey monkeyInputs
    let monkeyRounds = iterate playRound monkeys
    let round20Result = monkeyRounds !! 20
    putStr $ unlines (map show round20Result)
    let activeMonkeys = take 2 $ reverse $ sort round20Result
    print $ product (map inspectedItems activeMonkeys)
