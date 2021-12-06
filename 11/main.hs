import Data.List.Split (splitOn)

readNumbers :: FilePath -> IO [Int]
readNumbers path = do   contents <- readFile path
                        return $ map read $ splitOn "," contents

spawnFish :: [Int] -> [Int]
spawnFish [] = []
spawnFish (fish:moreFish) 
    | fish == 0 = 6 : 8 : spawnFish moreFish
    | otherwise = fish - 1 : spawnFish moreFish

main :: IO ()
main = do   numbers <- readNumbers "input.txt"
            let fish = iterate spawnFish numbers !! 80
            print $ length fish