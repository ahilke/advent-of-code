module Main where

import Constants (Move, Outcome, scoreMove, scoreOutcome, toMove, toOutcome)
import Paths_advent_of_code (getDataFileName)

readLines :: IO FilePath -> IO [String]
readLines path = do
    filePath <- path
    contents <- readFile filePath
    return (lines contents)

type MyMove = Move

type OppMove = Move

chooseMove :: OppMove -> Outcome -> MyMove
chooseMove "Rock" "Loss" = "Scissor"
chooseMove "Rock" "Draw" = "Rock"
chooseMove "Rock" "Win" = "Paper"
chooseMove "Paper" "Loss" = "Rock"
chooseMove "Paper" "Draw" = "Paper"
chooseMove "Paper" "Win" = "Scissor"
chooseMove "Scissor" "Loss" = "Paper"
chooseMove "Scissor" "Draw" = "Scissor"
chooseMove "Scissor" "Win" = "Rock"
chooseMove oppMove outcome = error $ "invalid input choosing a move: " ++ oppMove ++ ", " ++ outcome

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

calculateScore :: [[Move]] -> Int
calculateScore moves = sum $ map score scoreInput
  where
    score (myMove, outcome) = scoreGame myMove outcome
    outcomes = map play moves
      where
        play [oppMove, myMove] = playGame oppMove myMove
        play _ = error "invalid play"
    scoreInput = zip myMoves outcomes
      where
        myMoves = map getMyMove moves
        getMyMove gameMoves = gameMoves !! 1

main :: IO ()
main = do
    input <- readLines (getDataFileName "2022/02_rock_paper_scissors/input.txt")
    let codedMoves = map words input
    let moves = map (map toMove) codedMoves
    let score1 = calculateScore moves
    print score1
    let moveAndOutcome = map toMoves codedMoves
          where
            toMoves :: [String] -> [String]
            toMoves (oppMove:outcome:_) = [toMove oppMove, chooseMove (toMove oppMove) (toOutcome outcome)]
            toMoves _ = error "invalid toMoves"
    let score2 = calculateScore moveAndOutcome
    print score2
