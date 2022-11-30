readLines :: FilePath -> IO [String]
readLines path = do contents <- readFile path
                    return (lines contents)

toInt :: [String] -> [Int]
toInt strings = [read string | string <- strings]

incrementIfBigger :: Int -> Int -> Int -> Int
incrementIfBigger counter a b
    | b > a = counter + 1
    | otherwise = counter

countIncreases :: Int -> [Int] -> Int
countIncreases accumulator (a:b:c:d:rest) = countIncreases (incrementIfBigger accumulator (a+b+c) (b+c+d)) (b:c:d:rest)
countIncreases accumulator (a:rest) = accumulator
countIncreases accumulator [] = accumulator

main :: IO ()
main = do lines <- readLines "measurements.txt"
          print (show (countIncreases 0 (toInt lines)))
