module Main where

import Paths_advent_of_code (getDataFileName)
import Read (readLines)
import Tree (Branch(..), Leaf(..), Node(..), addNodeToTreeBy, findAllNodesBy)

import Data.List (sort)
import Data.Maybe (fromJust)
import Debug.Trace (trace)
import Text.Printf (printf)

-- TODO rewrite this file with function composition instead of excessive parentheses
--
data File =
    File
        { fileName :: String
        , size :: Int
        }
    deriving (Show)

newtype Dir =
    Dir
        { dirName :: String
        }
    deriving (Show)

type FileSystemBranch = Branch Dir File

type FileSystemLeaf = Leaf File

type FileSystemNode = Node Dir File

type Path = [String]

outputLineToNode :: (String, String) -> FileSystemNode
outputLineToNode (a, b) =
    if a == "dir"
        then NodeBranch (Branch {branchValue = (Dir {dirName = b}), children = []})
        else NodeLeaf (Leaf {leafValue = (File {fileName = b, size = read a})})

parseCd :: Path -> String -> Path
parseCd _ "/" = []
parseCd path ".." = init path
parseCd path dir = path ++ [dir]

processLine :: (FileSystemNode, Path) -> [String] -> (FileSystemNode, Path)
processLine (tree, path) ["$", "cd", cdInput] = (tree, parseCd path cdInput)
processLine (tree, path) ["$", "ls"] = (tree, path)
processLine (tree, path) [a, b] = (newTree, path)
  where
    node = outputLineToNode (a, b)
    newTree = fromJust $ addNodeToTreeBy tree hasName path node
processLine _ line = error $ "error processing line: " ++ unwords line

hasName :: String -> FileSystemNode -> Bool
hasName name (NodeLeaf leaf) = fileName (leafValue leaf) == name
hasName name (NodeBranch branch) = dirName (branchValue branch) == name

dirSize :: FileSystemBranch -> Int
dirSize input = foldl counter 0 (children input)
  where
    counter :: Int -> FileSystemNode -> Int
    counter acc (NodeLeaf leaf) = acc + size (leafValue leaf)
    counter acc (NodeBranch branch) = acc + dirSize branch

nodeSize :: FileSystemNode -> Int
nodeSize (NodeLeaf leaf) = size (leafValue leaf)
nodeSize (NodeBranch branch) = dirSize branch

main :: IO ()
main = do
    input <- readLines (getDataFileName "2022/07_no_space_left_on_device/input.txt")
    let inputWords = map words input
    let debugProcessLine (tree, path) debugInput =
            trace
                (printf "process line %s with path %s and tree %s" (show debugInput) (show path) (show tree))
                (processLine (tree, path) debugInput)
    let rootTree = NodeBranch $ Branch {branchValue = (Dir {dirName = "root"}), children = []}
    let (tree, _) = foldl debugProcessLine (rootTree, []) inputWords
    let smallDirs = findAllNodesBy tree smallDir []
          where
            smallDir :: FileSystemNode -> Bool
            smallDir (NodeLeaf _) = False
            smallDir (NodeBranch branch) = dirSize branch <= (100 * 1000)
    let smallDirSizes = map nodeSize smallDirs
    print $ sum smallDirSizes -- expected: 1350966
    let diskSize = 70 * 1000 * 1000
    let requiredSize = 30 * 1000 * 1000
    let occupied = nodeSize tree
    let free = diskSize - occupied
    let bigDirs = findAllNodesBy tree bigDir []
          where
            bigDir :: FileSystemNode -> Bool
            bigDir (NodeLeaf _) = False
            bigDir (NodeBranch branch) = dirSize branch >= requiredSize - free
    let bigDirSizes = sort $ map nodeSize bigDirs
    print $ head bigDirSizes -- expected: 6296435
