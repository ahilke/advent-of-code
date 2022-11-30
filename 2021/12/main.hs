import Data.List.Split (splitOn)
import Data.Sequence (fromList, Seq, adjust, lookup)
import Data.Maybe (fromJust)

readNumbers :: FilePath -> IO [Int]
readNumbers path = do   contents <- readFile path
                        return $ map read $ splitOn "," contents

putInBuckets :: [Int] -> Seq Int -> Seq Int
putInBuckets fish buckets = foldr (adjust (+ 1)) buckets fish

spawnFish :: Int -> Seq Int -> Seq Int
spawnFish index buckets 
    -- put fish into bucket 9 temporarily
    -- after we've processed the other buckets, we'll put them into bucket 6 and 8 where they belong
    | index == 0 = spawnFish (index + 1) $ adjust (const value) 9 buckets
    -- exit condition
    | index == 9 = adjust (+ value) 6 $ adjust (const value) 8 $ adjust (const 0) 9 buckets
    | otherwise = spawnFish (index + 1) $ adjust (const value) (index - 1) buckets
    where 
        value = fromJust $ Data.Sequence.lookup index buckets

main :: IO ()
main = do   fish <- readNumbers "input.txt"
            let buckets = putInBuckets fish (fromList $ replicate 10 0) 
            let allTheFish = iterate (spawnFish 0) buckets !! 256
            print $ sum allTheFish
