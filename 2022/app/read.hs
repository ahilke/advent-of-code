module Read
    ( readLines
    , readSingleLine
    ) where

readLines :: IO FilePath -> IO [String]
readLines path = do
    contents <- readSingleLine path
    return (lines contents)

readSingleLine :: IO FilePath -> IO String
readSingleLine path = do
    filePath <- path
    readFile filePath
