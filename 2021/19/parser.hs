module Parser (parse) where
import Debug.Trace

-- returns True for complete lines, False for incomplete lines, and points for corrupt lines
parse :: [Char] -> [Char] -> Either Bool Int
-- parse stack line | trace (show (stack, line)) False = undefined
parse stack line
    | null line && null stack = Left True
    | null line = Left False
    | nextChar `elem` "([{<" = parse (nextChar:stack) remainingLine
    | [nextMatch, nextChar] `elem` ["()", "[]", "{}", "<>"] = parse remainingStack remainingLine
    | nextChar == ')' = Right 3
    | nextChar == ']' = Right 57
    | nextChar == '}' = Right 1197
    | nextChar == '>' = Right 25137
    | otherwise = error "parse error"
    where 
        (nextChar:remainingLine) = line
        (nextMatch:remainingStack) = stack
