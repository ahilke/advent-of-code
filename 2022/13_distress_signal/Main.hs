module Main where

import Paths_advent_of_code (getDataFileName)
import Read (readLines)
import Tree (Branch(..), Leaf(..), Node(..), addNodeToTree)

import Data.Char (isDigit)
import Data.List (elemIndices)
import Data.Maybe (fromJust, isJust)

data Empty =
    Empty
    deriving (Show)

type PackageBranch = Branch Empty Int

type Package = Node Empty Int

-- | Converts an input line into a package tree.
-- |
-- | For example, "[[1],[2,3,4]]" is parsed into:
-- | .
-- | .      .
-- | 1      2 3 4
parseLine :: Int -> [Int] -> Package -> String -> Package
parseLine currentChild currentPath acc line
    -- | done parsing
    | null line = acc
    -- | comma contains no new information since when parsing a number we read till the next comma -> ignore
    | head line == ',' = parseLine currentChild currentPath acc (tail line)
    -- | create a new branch/list in the current position, reset child/list position
    | head line == '[' = parseLine 0 (currentPath ++ [currentChild]) withNewList (tail line)
    -- | end current branch/list, reset child/list position to previous plus one for the just processed child/list
    | head line == ']' = parseLine (last currentPath + 1) (init currentPath) acc (tail line)
    -- | parse number and add to current branch/list
    | otherwise = parseLine (currentChild + 1) currentPath withNewInt otherChars
  where
    (nextNumber, otherChars) = span isDigit line
    withNewInt = fromJust $ addNodeToTree acc currentPath (NodeLeaf (Leaf (read nextNumber)))
    withNewList = fromJust $ addNodeToTree acc currentPath (NodeBranch (Branch {children = [], branchValue = Empty}))

toTuples :: [a] -> [(a, a)]
toTuples [] = []
toTuples [_] = error "odd number of elements"
toTuples (x:y:rest) = (x, y) : toTuples rest

isPackageInRightOrder :: (Package, Package) -> Maybe Bool
isPackageInRightOrder (NodeLeaf a, NodeLeaf b) =
    if valueA == valueB
        then Nothing
        else Just (valueA < valueB)
  where
    valueA = leafValue a
    valueB = leafValue b
isPackageInRightOrder (NodeBranch a, NodeBranch b) = isListInRightOrder (children a, children b)
isPackageInRightOrder (NodeLeaf a, NodeBranch b) = isListInRightOrder ([NodeLeaf a], children b)
isPackageInRightOrder (NodeBranch a, NodeLeaf b) = isListInRightOrder (children a, [NodeLeaf b])

isListInRightOrder :: ([Package], [Package]) -> Maybe Bool
isListInRightOrder (a, b)
    | null a && null b = Nothing
    | null a = Just True
    | null b = Just False
    | isJust isFirstInRightOrder = isFirstInRightOrder
    | otherwise = isRestInRightOrder
  where
    isFirstInRightOrder = isPackageInRightOrder (head a, head b)
    isRestInRightOrder = isListInRightOrder (tail a, tail b)

main :: IO ()
main = do
    input <- readLines (getDataFileName "2022/13_distress_signal/input.txt")
    let emptyPackage = NodeBranch (Branch {branchValue = Empty, children = []})
    let packagePairs = toTuples $ map (parseLine 0 [] emptyPackage) (filter (not . null) input)
    let correctlyOrdered = map isPackageInRightOrder packagePairs
    let correctIndices = elemIndices (Just True) correctlyOrdered
    let oneIndexedIndices = map (+ 1) correctIndices
    print $ sum oneIndexedIndices
