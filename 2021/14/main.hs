import Data.List.Split (splitOn)

readNumbers :: FilePath -> IO [Int]
readNumbers path = do   contents <- readFile path
                        return $ map read $ splitOn "," contents

getFuelCost :: [Int] -> Int -> Int
getFuelCost positions target = foldr adder 0 positions
    where 
        adder :: Int -> Int -> Int
        adder position acc = acc + triangularNumber (abs (position - target))
        triangularNumber :: Int -> Int
        triangularNumber n = div (n * (n + 1)) 2

main :: IO ()
main = do   positions <- readNumbers "input.txt"
            let min = minimum positions
                max = maximum positions
                possibleTargets = [min..max]
                fuelCostsByTarget = map (getFuelCost positions) possibleTargets 

            print $ minimum fuelCostsByTarget
