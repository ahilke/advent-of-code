readLines :: FilePath -> IO [String]
readLines path = do contents <- readFile path
                    return (lines contents)

toWords :: [String] -> [[String]]
toWords lines = [words line | line <- lines]

toDirections :: [[String]] -> [(String, Int)]
toDirections words = [(direction, read value) | [direction, value] <- words]

addDirection :: (Int, Int) -> (String, Int) -> (Int, Int)
addDirection (pos, depth) ("forward", value) = (pos + value, depth)
addDirection (pos, depth) ("down", value) = (pos, depth + value)
addDirection (pos, depth) ("up", value) = (pos, depth - value)
addDirection _ _ = error "unexpected input"

sumDirections :: [(String, Int)] -> (Int, Int)
sumDirections = foldl addDirection (0, 0)

main :: IO ()
main = do lines <- readLines "directions.txt"
          let directions = toDirections (toWords lines)
          let (pos, depth) = sumDirections directions
          print $ pos * depth
