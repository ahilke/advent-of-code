module Main where

import List (replace)
import Paths_advent_of_code (getDataFileName)
import Read (readLines)

import Data.Ix (range)
import Data.List (sort)
import Data.List.Split (splitOn)
import Text.Printf (printf)

data Monkey =
    Monkey
        { items :: [Item]
        , inspectedItems :: Int
        , operation :: Int -> Int
        , test :: Int -> Int
        , testNumber :: Int
        }

data Item =
    Item
        { itemValue :: Int -- part 1 only uses this
        , remainders :: [Int] -- part 2 only uses this
        }

createItem :: Int -> Item
createItem input = Item {itemValue = input, remainders = repeat input}

instance Show Monkey where
    show monkey = printf "inspected items: %d" (inspectedItems monkey)

instance Eq Monkey where
    a == b = inspectedItems a == inspectedItems b

instance Ord Monkey where
    compare a b = compare (inspectedItems a) (inspectedItems b)

readMonkey :: [String] -> Monkey
readMonkey input =
    Monkey {items = initItems, inspectedItems = 0, operation = initOperation, test = initTest, testNumber = divisor}
  where
    itemLine = input !! 1
    itemString = splitOn ": " itemLine !! 1 -- e.g. "79, 98"
    initItems = map (createItem . read) (splitOn ", " itemString)
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

updateWorry :: Monkey -> Item -> Item
updateWorry monkey value = value {itemValue = div (operation monkey (itemValue value)) 3}

updateWorryDiv :: [Monkey] -> Monkey -> Item -> Item
updateWorryDiv monkeys monkey value = value {remainders = newRemainders}
  where
    divisors = map testNumber monkeys
    oldRemainders = remainders value
    updateOperation = operation monkey
    newRemainders = zipWith operationRemainder oldRemainders divisors
      where
        operationRemainder remainder divisor = rem (updateOperation remainder) divisor

processItem :: (Monkey -> Item -> Item) -> Int -> [Monkey] -> Item -> [Monkey]
processItem worryFunction monkeyIndex monkeys worry
    -- trace (printf "worry: %d -> %d, monkey: %d -> %d" worry newWorry monkeyIndex newMonkeyIndex)
 = replace newMonkeyWithItem newMonkeyIndex (replace oldMonkeyWithoutItem monkeyIndex monkeys)
  where
    oldMonkey = monkeys !! monkeyIndex
    newWorry = worryFunction oldMonkey worry
    newMonkeyIndex = test oldMonkey (remainders newWorry !! monkeyIndex)
    newMonkey = monkeys !! newMonkeyIndex
    oldMonkeyWithoutItem = oldMonkey {items = tail (items oldMonkey), inspectedItems = inspectedItems oldMonkey + 1}
    newMonkeyWithItem = newMonkey {items = items newMonkey ++ [newWorry]}

processMonkey :: (Monkey -> Item -> Item) -> [Monkey] -> Int -> [Monkey]
processMonkey worryFunction monkeys monkeyIndex = foldl (processItem worryFunction monkeyIndex) monkeys (items monkey)
  where
    monkey = monkeys !! monkeyIndex

playRound :: (Monkey -> Item -> Item) -> [Monkey] -> [Monkey]
playRound worryFunction monkeys = foldl (processMonkey worryFunction) monkeys (range (0, length monkeys - 1))

main :: IO ()
main = do
    input <- readLines (getDataFileName "2022/11_monkey_in_the_middle/input.txt")
    let monkeyInputs = splitOn [""] input
    let monkeys = map readMonkey monkeyInputs
    let monkeyRounds = iterate (playRound updateWorry) monkeys
    let round20Result = monkeyRounds !! 20
    putStr $ unlines (map show round20Result)
    let activeMonkeys = take 2 $ reverse $ sort round20Result
    print $ product (map inspectedItems activeMonkeys)
    let monkeyRoundsDiv = iterate (playRound (updateWorryDiv monkeys)) monkeys
    putStr $ unlines (map show (monkeyRoundsDiv !! 20))
    putStr $ unlines (map show (monkeyRoundsDiv !! 1000))
    let round10kResultDiv = monkeyRoundsDiv !! 10000
    let activeMonkeysDiv = take 2 $ reverse $ sort round10kResultDiv
    print $ product (map inspectedItems activeMonkeysDiv)
