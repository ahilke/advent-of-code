module Diagnostic (toGamma, toEpsilon) where
import Data.Char (intToDigit)
import Bin (binToDec)

toGamma :: Int -> [Int] -> Int
toGamma length ones = let digits = map (mostCommonBit length) ones
                      in binToDec (map intToDigit digits)
                      
toEpsilon :: Int -> [Int] -> Int
toEpsilon length ones = let digits = map (invert . mostCommonBit length) ones
                        in binToDec (map intToDigit digits)

invert :: Int -> Int
invert 0 = 1
invert 1 = 0
invert _ = error "not a bit!"

mostCommonBit :: Int -> Int -> Int 
mostCommonBit linesLength ones
    | fOnes > fLinesLength / 2 = 1
    | fOnes < fLinesLength / 2 = 0
    | otherwise = error "exactly half!"
    where 
        fOnes = fromIntegral ones
        fLinesLength = fromIntegral linesLength
    