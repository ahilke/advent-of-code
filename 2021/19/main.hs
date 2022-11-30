import Input (readLines)
import Parser (parse)
import Data.Either (fromRight)

addResults :: [Either Bool Int] -> Int
addResults = foldr adder 0
    where
        adder :: Either Bool Int -> Int -> Int
        adder result acc = acc + value
            where value = fromRight 0 result

main :: IO ()
main = do   lines <- readLines "input.txt"
            let parsed = map (parse []) lines
                result = addResults parsed

            print result
