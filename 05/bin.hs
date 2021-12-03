module Bin(binToDec) where

import Data.Char (digitToInt)

binToDec :: String -> Int
binToDec = foldl addDigit 0 

addDigit :: Int -> Char -> Int
addDigit acc digit = acc * 2 + digitToInt digit
