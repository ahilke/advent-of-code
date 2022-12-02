module Constants
  ( toMove
  , scoreMove
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

scoreOutcome :: Outcome -> Int
scoreOutcome "Loss" = 0
scoreOutcome "Draw" = 3
scoreOutcome "Win" = 6
scoreOutcome _ = error "invalid outcome"
