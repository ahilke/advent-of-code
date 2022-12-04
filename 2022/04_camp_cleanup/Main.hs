module Main where

import Paths_advent_of_code (getDataFileName)
import Read (readLines)

import Data.List.Split (splitOn)

firstContainsSecond :: (Int, Int) -> (Int, Int) -> Bool
firstContainsSecond (aMin, aMax) (bMin, bMax) = aMin <= bMin && aMax >= bMax

oneContainsOther :: (Int, Int) -> (Int, Int) -> Bool
oneContainsOther a b = firstContainsSecond a b || firstContainsSecond b a

inRange :: Int -> (Int, Int) -> Bool
inRange a (x, y) = a >= x && a <= y

overlaps :: (Int, Int) -> (Int, Int) -> Bool
overlaps (aMin, aMax) b = aMin `inRange` b || aMax `inRange` b || firstContainsSecond (aMin, aMax) b

-- parses "a-b" into (a, b)
parseAssignment :: String -> (Int, Int)
parseAssignment assignment =
    case splitOn "-" assignment of
        (strA:strB:_) -> (read strA, read strB)
        _ -> error $ "parsing assignment: " ++ assignment

main :: IO ()
main = do
    input <- readLines (getDataFileName "2022/04_camp_cleanup/input.txt")
    let pairs = map (splitOn ",") input
    let parsed = map (map parseAssignment) pairs
    let containedCount = foldl counter 0 parsed
          where
            counter :: Int -> [(Int, Int)] -> Int
            counter count (a:b:_) =
                (if oneContainsOther a b
                     then count + 1
                     else count)
            counter _ x = error $ "counting contained assignments: " ++ show x
    print containedCount
    let overlapCount = foldl counter 0 parsed
          where
            counter :: Int -> [(Int, Int)] -> Int
            counter count (a:b:_) =
                (if overlaps a b
                     then count + 1
                     else count)
            counter _ x = error $ "counting overlaps: " ++ show x
    print overlapCount
