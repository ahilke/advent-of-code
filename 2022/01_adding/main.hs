import Data.List (sort)
import Data.List.Split (splitOn)

readLines :: FilePath -> IO [String]
readLines path = do contents <- readFile path
                    return (lines contents)

toInt :: [String] -> [Int]
toInt strings = [read string | string <- strings]

main :: IO ()
main = do lines <- readLines "input.txt"
          let inventories = splitOn [""] lines
          let parsedInventories = map toInt inventories
          let sums = map sum parsedInventories
          let sortedSums = reverse $ sort $ map sum parsedInventories

          let result1 = head sortedSums
          let result2 = sum $ take 3 sortedSums

          print result1
          print result2


