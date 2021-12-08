module Input (readLines) where

import Data.List.Split (splitOn)
import Data.List (sort)

readLines :: FilePath -> IO [([String], [String])]
readLines path = do contents <- readFile path
                    return (map splitLine (lines contents))
                        where
                            splitLine :: String -> ([String], [String])
                            splitLine line = (x, y)
                                where 
                                    [x, y] = map (map sort . words) (splitOn " | " line)
                                   