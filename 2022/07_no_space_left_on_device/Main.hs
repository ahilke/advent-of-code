module Main where

import Paths_advent_of_code (getDataFileName)
import Read (readLines)

import Data.Foldable (find)
import Data.List (sort)
import Data.Maybe (fromJust)
import Debug.Trace (trace)
import Text.Printf (printf)

data File =
    File
        { fileName :: String
        , size :: Int
        }
    deriving (Show)

data Dir =
    Dir
        { dirName :: String
        , nodes :: [Node]
        }
    deriving (Show)

data Node
    = NodeFile File
    | NodeDir Dir
    deriving (Show)

data Tree
    = EmptyTree
    | TreeNode Node
    deriving (Show)

type Path = [String]

outputLineToNode :: (String, String) -> Node
outputLineToNode (a, b) =
    if a == "dir"
        then NodeDir (Dir {dirName = b, nodes = []})
        else NodeFile (File {fileName = b, size = read a})

parseCd :: Path -> String -> Path
parseCd _ "/" = []
parseCd path ".." = init path
parseCd path dir = path ++ [dir]

processLine :: (Tree, Path) -> [String] -> (Tree, Path)
processLine (tree, path) ["$", "cd", cdInput] = (tree, parseCd path cdInput)
processLine (tree, path) ["$", "ls"] = (tree, path)
processLine (tree, path) [a, b] = (newTree, path)
  where
    node = outputLineToNode (a, b)
    newTree = addNodeToTree tree path node
processLine _ line = error $ "error processing line: " ++ unwords line

addNodeToTree :: Tree -> Path -> Node -> Tree
addNodeToTree tree path node = replaceDir tree path (addNodeToDir (findDir tree path) node)

addNodeToDir :: Dir -> Node -> Dir
addNodeToDir dir node = dir {nodes = nodes dir ++ [node]}

getTreeNode :: Tree -> Node
getTreeNode EmptyTree = error "getTreeNode on empty tree"
getTreeNode (TreeNode node) = node

replaceElementInList :: [a] -> a -> (a -> Bool) -> [a]
replaceElementInList [] _ _ = error "did not find element to replace"
replaceElementInList (x:rest) replacement predicate =
    if predicate x
        then replacement : rest
        else x : replaceElementInList rest replacement predicate

findDir :: Tree -> Path -> Dir
findDir tree [] =
    case tree of
        EmptyTree -> error "path does not exist"
        TreeNode (NodeFile file) -> error $ "path ends at file" ++ fileName file
        TreeNode (NodeDir dir) -> dir
findDir tree (pathSegment:subPath) =
    case tree of
        EmptyTree -> error "path does not exist"
        TreeNode (NodeFile file) -> error $ "path ends at file" ++ fileName file
        TreeNode (NodeDir dir) -> findDir subTree subPath
            where subTree = TreeNode (findNodeInList pathSegment (nodes dir))

replaceDir :: Tree -> Path -> Dir -> Tree
replaceDir tree [] newDir =
    case tree of
        EmptyTree -> error "path does not exist"
        TreeNode (NodeFile file) -> error $ "path ends at file" ++ fileName file
        TreeNode (NodeDir _) -> TreeNode (NodeDir newDir)
replaceDir tree (pathSegment:subPath) newDir =
    case tree of
        EmptyTree -> error "path does not exist"
        TreeNode (NodeFile file) -> error $ "path ends at file" ++ fileName file
        TreeNode (NodeDir dir) -> TreeNode $ NodeDir $ dir {nodes = newNodes}
            where subTree = TreeNode (findNodeInList pathSegment (nodes dir))
                  newSubTree = replaceDir subTree subPath newDir
                  newSubTreeNode = getTreeNode newSubTree
                  hasDirName = hasName pathSegment
                  newNodes = replaceElementInList (nodes dir) newSubTreeNode hasDirName

findNodeInList :: String -> [Node] -> Node
findNodeInList name list = fromJust $ find (hasName name) list

findAllNodesInTree :: Tree -> [Node] -> (Node -> Bool) -> [Node]
findAllNodesInTree EmptyTree matchedNodes _ = matchedNodes
findAllNodesInTree (TreeNode node) matchedNodes predicate = matchedNodes ++ maybeNode ++ matchedChildNodes
  where
    maybeNode = [node | predicate node]
    matchedChildNodes =
        case node of
            NodeFile _ -> []
            NodeDir dir -> concatMap mapper (nodes dir)
                where mapper child = findAllNodesInTree (TreeNode child) [] predicate

hasName :: String -> Node -> Bool
hasName name (NodeFile file) = fileName file == name
hasName name (NodeDir dir) = dirName dir == name

dirSize :: Dir -> Int
dirSize input = foldl counter 0 (nodes input)
  where
    counter acc (NodeFile file) = acc + size file
    counter acc (NodeDir dir) = acc + dirSize dir

nodeSize :: Node -> Int
nodeSize (NodeFile file) = size file
nodeSize (NodeDir dir) = dirSize dir

treeSize :: Tree -> Int
treeSize EmptyTree = 0
treeSize (TreeNode node) = nodeSize node

main :: IO ()
main = do
    input <- readLines (getDataFileName "2022/07_no_space_left_on_device/input.txt")
    let inputWords = map words input
    let debugProcessLine (tree, path) debugInput =
            trace
                (printf "process line %s with path %s and tree %s" (show debugInput) (show path) (show tree))
                (processLine (tree, path) debugInput)
    let rootTree = TreeNode $ NodeDir $ Dir {dirName = "root", nodes = []}
    let (tree, _) = foldl debugProcessLine (rootTree, []) inputWords
    let smallDirs = findAllNodesInTree tree [] smallDir
          where
            smallDir (NodeFile _) = False
            smallDir (NodeDir dir) = dirSize dir <= (100 * 1000)
    let smallDirSizes = map nodeSize smallDirs
    print $ sum smallDirSizes
    let diskSize = 70 * 1000 * 1000
    let requiredSize = 30 * 1000 * 1000
    let occupied = treeSize tree
    let free = diskSize - occupied
    let bigDirs = findAllNodesInTree tree [] bigDir
          where
            bigDir (NodeFile _) = False
            bigDir (NodeDir dir) = dirSize dir >= requiredSize - free
    let bigDirSizes = sort $ map nodeSize bigDirs
    print $ head bigDirSizes
