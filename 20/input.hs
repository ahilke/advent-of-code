module Input (readLines) where

readLines :: FilePath -> IO [String]
readLines path = do content <- readFile path
                    return $ lines content
                                   