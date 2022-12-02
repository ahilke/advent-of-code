module Constants
    ( toMove
    , scoreMove
    , toOutcome
    , scoreOutcome
    , Move
    , Outcome
    ) where

type Move = String

toMove :: String -> Move
toMove move
    | move == "A" || move == "X" = "Rock"
    | move == "B" || move == "Y" = "Paper"
    | move == "C" || move == "Z" = "Scissor"
    | otherwise = error $ "invalid move: " ++ move

scoreMove :: Move -> Int
scoreMove "Rock" = 1
scoreMove "Paper" = 2
scoreMove "Scissor" = 3
scoreMove move = error $ "invalid move: " ++ move

type Outcome = String

toOutcome :: String -> Outcome
toOutcome "X" = "Loss"
toOutcome "Y" = "Draw"
toOutcome "Z" = "Win"
toOutcome outcome = error $ "invalid outcome: " ++ outcome

scoreOutcome :: Outcome -> Int
scoreOutcome "Loss" = 0
scoreOutcome "Draw" = 3
scoreOutcome "Win" = 6
scoreOutcome _ = error "invalid outcome"
