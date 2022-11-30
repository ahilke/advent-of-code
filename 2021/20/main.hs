import Input (readLines)
import Parser (parse)
import Data.Either (fromRight, partitionEithers)
import Numeric.Statistics.Median (median)

main :: IO ()
main = do   lines <- readLines "input.txt"
            let parsed = map (parse []) lines
                incompleteResults = snd $ partitionEithers parsed

            print $ round $ median $ map fromIntegral incompleteResults
