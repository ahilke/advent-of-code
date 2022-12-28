module Tree
    ( Branch(..)
    , Leaf(..)
    , Node(..)
    , Tree(..)
    , addNodeToTree
    , findAllNodesBy
    ) where

import List (replaceElement)

import Data.List (find)
import Data.Maybe (fromJust)
import Text.Printf (printf)

-- TODO: split into core and utility (like findAllNodesBy)
-- TODO: add tests for these functions
--
data Branch tBranchValue tLeafValue =
    Branch
        { branchValue :: tBranchValue
        , children :: [Node tBranchValue tLeafValue]
        }
    deriving (Show)

newtype Leaf tLeafValue =
    Leaf
        { leafValue :: tLeafValue
        }
    deriving (Show)

data Node tBranchValue tLeafValue
    = NodeBranch (Branch tBranchValue tLeafValue)
    | NodeLeaf (Leaf tLeafValue)
    deriving (Show)

-- todo: add getNodeValue to Node? https://stackoverflow.com/a/6119257/10380981
-- getNodeValue :: Node a b -> (a|b)
-- getNodeValue (NodeBranch branch) = branchValue branch
-- getNodeValue (NodeLeaf leaf) = leafValue leaf
--
--
-- | A general tree with any number of children per node.
-- |
-- | If you do not need a different type for leaves, use just the branch type to be able to add children to any node
data Tree tBranchValue tLeafValue
    = EmptyTree
    | TreeNode (Node tBranchValue tLeafValue)
    deriving (Show)

type NodePredicate tBranchValue tLeafValue = Node tBranchValue tLeafValue -> Bool

type PathPredicate tPathSegment tBranchValue tLeafValue = tPathSegment -> NodePredicate tBranchValue tLeafValue

getRootNode :: Tree tBranchValue tLeafValue -> Node tBranchValue tLeafValue
getRootNode EmptyTree = error "Tree is empty."
getRootNode (TreeNode node) = node

findChild :: Branch tBranchValue tLeafValue -> NodePredicate tBranchValue tLeafValue -> Node tBranchValue tLeafValue
findChild branch predicate = fromJust $ find predicate (children branch)

-- todo: do not export this (and possible some other non-needed functions)
appendChild :: Branch tBranchValue tLeafValue -> Node tBranchValue tLeafValue -> Branch tBranchValue tLeafValue
appendChild branch node = branch {children = children branch ++ [node]}

addNodeToTree ::
       Show tLeafValue
    => Show tPathSegment =>
           Tree tBranchValue tLeafValue -> PathPredicate tPathSegment tBranchValue tLeafValue -> [tPathSegment] -> Node tBranchValue tLeafValue -> Tree tBranchValue tLeafValue
addNodeToTree tree pathPredicate path node = replaceNode tree pathPredicate path (NodeBranch (appendChild branch node))
  where
    nodeAtPath = findNode tree pathPredicate path
    branch =
        case nodeAtPath of
            NodeLeaf leafAtPath -> error $ printf "Cannot append to leaf. Leaf: %s." (show leafAtPath)
            NodeBranch branchAtPath -> branchAtPath

findNode ::
       Show tLeafValue
    => Show tPathSegment =>
           Tree tBranchValue tLeafValue -> PathPredicate tPathSegment tBranchValue tLeafValue -> [tPathSegment] -> Node tBranchValue tLeafValue
findNode tree _ [] =
    case tree of
        EmptyTree -> error "No node for given path."
        TreeNode node -> node
findNode tree pathPredicate (pathSegment:subPath) =
    case tree of
        EmptyTree -> error "No node for given path."
        TreeNode (NodeLeaf leaf) ->
            error $
            printf
                "Path prematurely ends at leaf. Leaf: %s. Remaining path: %s"
                (show leaf)
                (show (pathSegment : subPath))
        TreeNode (NodeBranch branch) -> findNode subTree pathPredicate subPath
            where nodePredicate = pathPredicate pathSegment
                  subTree = TreeNode (findChild branch nodePredicate)

findAllNodesBy ::
       Tree tBranchValue tLeafValue
    -> NodePredicate tBranchValue tLeafValue
    -> [Node tBranchValue tLeafValue]
    -> [Node tBranchValue tLeafValue]
findAllNodesBy EmptyTree _ matchedNodes = matchedNodes
findAllNodesBy (TreeNode root) predicate matchedNodes = matchedNodes ++ maybeRoot ++ matchedChildNodes
  where
    maybeRoot = [root | predicate root]
    matchedChildNodes =
        case root of
            NodeLeaf _ -> []
            NodeBranch branch -> concatMap findChildNodesBy (children branch)
                where findChildNodesBy child = findAllNodesBy (TreeNode child) predicate []

--
-- TODO: why is this type signature not broken into multiple lines? -> report hindent issue?
replaceNode ::
       Show tLeafValue
    => Show tPathSegment =>
           Tree tBranchValue tLeafValue -> PathPredicate tPathSegment tBranchValue tLeafValue -> [tPathSegment] -> Node tBranchValue tLeafValue -> Tree tBranchValue tLeafValue
replaceNode tree _ [] newNode =
    case tree of
        EmptyTree -> error "No node for given path."
        TreeNode _ -> TreeNode newNode
replaceNode tree pathPredicate (pathSegment:subPath) newNode =
    case tree of
        EmptyTree -> error "no node for given path"
        TreeNode (NodeLeaf leaf) ->
            error $
            printf
                "Path prematurely ends at leaf. Leaf: %s. Remaining path: %s"
                (show leaf)
                (show (pathSegment : subPath))
        TreeNode (NodeBranch branch) -> TreeNode $ NodeBranch $ branch {children = newChildren}
            where nodePredicate = pathPredicate pathSegment
                  subTree = TreeNode (findChild branch nodePredicate)
                  newSubTree = replaceNode subTree pathPredicate subPath newNode
                  newSubTreeRoot = getRootNode newSubTree
                  newChildren = replaceElement (children branch) nodePredicate newSubTreeRoot
