module Main where

import Paths_advent_of_code (getDataFileName)
import Read (readLines)
import Tree (Branch(..), Leaf(..), Node(..), addNodeToTree)

import Data.Char (isDigit)
import Data.List (elemIndex, elemIndices, sortBy)
import Data.Maybe (fromJust)

data Empty =
    Empty
    deriving (Show, Eq)

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

getPackageOrdering :: Package -> Package -> Ordering
getPackageOrdering (NodeLeaf a) (NodeLeaf b) =
    if valueA == valueB
        then EQ
        else compare valueA valueB
  where
    valueA = leafValue a
    valueB = leafValue b
getPackageOrdering (NodeBranch a) (NodeBranch b) = getListOrdering (children a) (children b)
getPackageOrdering (NodeLeaf a) (NodeBranch b) = getListOrdering ([NodeLeaf a]) (children b)
getPackageOrdering (NodeBranch a) (NodeLeaf b) = getListOrdering (children a) ([NodeLeaf b])

getListOrdering :: [Package] -> [Package] -> Ordering
getListOrdering a b
    | null a && null b = EQ
    | null a = LT
    | null b = GT
    | firstOrdering /= EQ = firstOrdering
    | otherwise = restOrdering
  where
    firstOrdering = getPackageOrdering (head a) (head b)
    restOrdering = getListOrdering (tail a) (tail b)

main :: IO ()
main = do
    input <- readLines (getDataFileName "2022/13_distress_signal/input.txt")
    let emptyPackage = NodeBranch (Branch {branchValue = Empty, children = []})
    let packages = map (parseLine 0 [] emptyPackage) (filter (not . null) input)
    let packagePairs = toTuples packages
    let packagePairOrderings = map getPairOrdering packagePairs
          where
            getPairOrdering (a, b) = getPackageOrdering a b
    let correctIndices = elemIndices LT packagePairOrderings
    let oneIndexedIndices = map (+ 1) correctIndices
    print $ sum oneIndexedIndices -- expected: 5252
    let divider1 = parseLine 0 [] emptyPackage "[[2]]"
    let divider2 = parseLine 0 [] emptyPackage "[[6]]"
    let sortedPackages = sortBy getPackageOrdering (divider1 : divider2 : packages)
    let divider1Index = fromJust (elemIndex divider1 sortedPackages) + 1 -- +1 to convert to 1-index
    let divider2Index = fromJust (elemIndex divider2 sortedPackages) + 1 -- +1 to convert to 1-index
    print $ divider1Index * divider2Index -- expected: 20592
