module Main where

import Constants (Move, Outcome, scoreMove, scoreOutcome, toMove)
import Paths_advent_of_code (getDataFileName)

readLines :: IO FilePath -> IO [String]
readLines path = do
  filePath <- path
  contents <- readFile filePath
  return (lines contents)

type MyMove = Move

type OppMove = Move

playGame :: OppMove -> MyMove -> Outcome
playGame "Rock" "Rock" = "Draw"
playGame "Rock" "Paper" = "Win"
playGame "Rock" "Scissor" = "Loss"
playGame "Paper" "Rock" = "Loss"
playGame "Paper" "Paper" = "Draw"
playGame "Paper" "Scissor" = "Win"
playGame "Scissor" "Rock" = "Win"
playGame "Scissor" "Paper" = "Loss"
playGame "Scissor" "Scissor" = "Draw"
playGame oppMove myMove = error $ "invalid game:" ++ oppMove ++ ", " ++ myMove

scoreGame :: MyMove -> Outcome -> Int
scoreGame myMove outcome = scoreMove myMove + scoreOutcome outcome

main :: IO ()
main = do
  input <- readLines (getDataFileName "2022/02_rock_paper_scissors/input.txt")
  let codedMoves = map words input
  let moves = map (map toMove) codedMoves
  let outcomes = map play moves
        where
          play [oppMove, myMove] = playGame oppMove myMove
          play _ = error "invalid play"
  let scoreInput = zip myMoves outcomes
        where
          myMoves = map getMyMove moves
          getMyMove gameMoves = gameMoves !! 1
  let totalScore = sum $ map score scoreInput
        where
          score (myMove, outcome) = scoreGame myMove outcome
  print totalScore
