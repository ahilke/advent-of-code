module Input (readLines, readVentLines, removeDiagonals) where

import Data.List.Split (splitOn)
import Map (VentLine)

readLines :: FilePath -> IO [String]
readLines path = do contents <- readFile path
                    return (lines contents)

readVentLines :: [String] -> [VentLine] -> [VentLine]
readVentLines lines boards = foldl readNext boards lines

readNext :: [VentLine] -> String -> [VentLine]
readNext lines line = lines ++ [((x1,y1),(x2,y2))]
                        where x1:y1:_ = map read $ splitOn "," start
                              x2:y2:_ = map read $ splitOn "," end
                              start:end:_ = splitOn " -> " line
                              
removeDiagonals :: [VentLine] -> [VentLine]
removeDiagonals ventLines
      | null ventLines = []
      | x1 /= x2 && y1 /= y2 = removeDiagonals otherLines
      | otherwise = line : removeDiagonals otherLines
      where 
            ((x1,y1),(x2,y2)) = line
            (line:otherLines) = ventLines
