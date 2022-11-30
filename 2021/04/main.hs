import Debug.Trace

readLines :: FilePath -> IO [String]
readLines path = do contents <- readFile path
                    return (lines contents)

toWords :: [String] -> [[String]]
toWords lines = [words line | line <- lines]

toDirections :: [[String]] -> [(String, Int)]
toDirections words = [(direction, read value) | [direction, value] <- words]

addDirection :: (Int, Int, Int) -> (String, Int) -> (Int, Int, Int)
-- addDirection acc value | trace ("addDirection " ++ show acc ++ " " ++ show value) False = undefined 
addDirection (pos, depth, aim) ("forward", value) = (pos + value, depth + value * aim, aim)
addDirection (pos, depth, aim) ("down", value) = (pos, depth, aim + value)
addDirection (pos, depth, aim) ("up", value) = (pos, depth, aim - value)
addDirection _ _ = error "unexpected input"

sumDirections :: [(String, Int)] -> (Int, Int, Int)
sumDirections = foldl addDirection (0, 0, 0)

main :: IO ()
main = do lines <- readLines "directions.txt"
          let directions = toDirections (toWords lines)
          let (pos, depth, _) = sumDirections directions
          print $ pos * depth
