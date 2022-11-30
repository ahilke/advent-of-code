module Parser (parse) where
import Debug.Trace

-- returns True for complete lines, False for corrupt lines, and points for incomplete lines
parse :: [Char] -> [Char] -> Either Bool Int
-- parse stack line | trace (show (stack, line)) False = undefined
parse stack line
    | null line && null stack = Left True
    | null line = Right $ score $ reverse stack
    | nextChar `elem` "([{<" = parse (nextChar:stack) remainingLine
    | [nextMatch, nextChar] `elem` ["()", "[]", "{}", "<>"] = parse remainingStack remainingLine
    | nextChar `elem` ")]}>" = Left False
    | otherwise = error "parse error"
    where 
        (nextChar:remainingLine) = line
        (nextMatch:remainingStack) = stack

score :: [Char] -> Int
score = foldr adder 0
    where 
        -- adder nextChar acc | trace (show (nextChar, acc)) False = undefined
        adder nextChar acc 
            | nextChar == '(' = acc * 5 + 1
            | nextChar == '[' = acc * 5 + 2
            | nextChar == '{' = acc * 5 + 3
            | nextChar == '<' = acc * 5 + 4
            | otherwise = error "score error"
