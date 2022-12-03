module Read
    ( readLines
    ) where

readLines :: IO FilePath -> IO [String]
readLines path = do
    filePath <- path
    contents <- readFile filePath
    return (lines contents)
