module Input (readLines) where

import Data.Char (digitToInt)
import Data.Matrix (Matrix, fromLists)

readLines :: FilePath -> IO (Matrix Int)
readLines path = do contents <- readFile path
                    let digits :: [[Char]]
                        digits = lines contents
                        numbers :: [[Int]]
                        numbers = map (map digitToInt) digits
                    return $ fromLists numbers
                                   