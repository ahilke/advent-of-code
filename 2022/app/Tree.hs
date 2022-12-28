-- | A general tree with any number of children per node.
-- |
-- | If you do not need a different type for leaves, use just the branch type to be able to add children to any node
module Tree
    ( Branch(..)
    , Leaf(..)
    , Node(..)
    , addNodeToTree
    , addNodeToTreeBy
    , findAllNodesBy
    ) where

import List (replace, replaceBy)

import Data.List (find)
import Data.Maybe (fromJust)
import Text.Printf (printf)

-- TODO: split into core and utility (like findAllNodesBy); split path predicate functions with suffix `by`
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
type NodePredicate tBranchValue tLeafValue = Node tBranchValue tLeafValue -> Bool

type PathPredicate tPathSegment tBranchValue tLeafValue = tPathSegment -> NodePredicate tBranchValue tLeafValue

findChild :: Branch tBranchValue tLeafValue -> NodePredicate tBranchValue tLeafValue -> Node tBranchValue tLeafValue
findChild branch predicate = fromJust $ find predicate (children branch)

-- todo: do not export this (and possible some other non-needed functions)
appendChild :: Branch tBranchValue tLeafValue -> Node tBranchValue tLeafValue -> Branch tBranchValue tLeafValue
appendChild branch node = branch {children = children branch ++ [node]}

addNodeToTree ::
       Show tLeafValue
    => Node tBranchValue tLeafValue
    -> [Int]
    -> Node tBranchValue tLeafValue
    -> Node tBranchValue tLeafValue
addNodeToTree tree path node = replaceNode tree path (NodeBranch (appendChild branch node))
  where
    nodeAtPath = findNode tree path
    branch =
        case nodeAtPath of
            NodeLeaf leafAtPath -> error $ printf "Cannot append to leaf. Leaf: %s." (show leafAtPath)
            NodeBranch branchAtPath -> branchAtPath

addNodeToTreeBy ::
       Show tLeafValue
    => Show tPathSegment =>
           Node tBranchValue tLeafValue -> PathPredicate tPathSegment tBranchValue tLeafValue -> [tPathSegment] -> Node tBranchValue tLeafValue -> Node tBranchValue tLeafValue
addNodeToTreeBy tree pathPredicate path node =
    replaceNodeBy tree pathPredicate path (NodeBranch (appendChild branch node))
  where
    nodeAtPath = findNodeBy tree pathPredicate path
    branch =
        case nodeAtPath of
            NodeLeaf leafAtPath -> error $ printf "Cannot append to leaf. Leaf: %s." (show leafAtPath)
            NodeBranch branchAtPath -> branchAtPath

findNode :: Show tLeafValue => Node tBranchValue tLeafValue -> [Int] -> Node tBranchValue tLeafValue
findNode tree [] = tree
findNode tree (pathSegment:subPath) =
    case tree of
        NodeLeaf leaf ->
            error $
            printf
                "Path prematurely ends at leaf. Leaf: %s. Remaining path: %s"
                (show leaf)
                (show (pathSegment : subPath))
        NodeBranch branch -> findNode subTree subPath
            where subTree = children branch !! pathSegment

findNodeBy ::
       Show tLeafValue
    => Show tPathSegment =>
           Node tBranchValue tLeafValue -> PathPredicate tPathSegment tBranchValue tLeafValue -> [tPathSegment] -> Node tBranchValue tLeafValue
findNodeBy tree _ [] = tree
findNodeBy tree pathPredicate (pathSegment:subPath) =
    case tree of
        NodeLeaf leaf ->
            error $
            printf
                "Path prematurely ends at leaf. Leaf: %s. Remaining path: %s"
                (show leaf)
                (show (pathSegment : subPath))
        NodeBranch branch -> findNodeBy subTree pathPredicate subPath
            where nodePredicate = pathPredicate pathSegment
                  subTree = findChild branch nodePredicate

findAllNodesBy ::
       Node tBranchValue tLeafValue
    -> NodePredicate tBranchValue tLeafValue
    -> [Node tBranchValue tLeafValue]
    -> [Node tBranchValue tLeafValue]
findAllNodesBy root predicate matchedNodes = matchedNodes ++ maybeRoot ++ matchedChildNodes
  where
    maybeRoot = [root | predicate root]
    matchedChildNodes =
        case root of
            NodeLeaf _ -> []
            NodeBranch branch -> concatMap findChildNodesBy (children branch)
                where findChildNodesBy child = findAllNodesBy child predicate []

replaceNode ::
       Show tLeafValue
    => Node tBranchValue tLeafValue
    -> [Int]
    -> Node tBranchValue tLeafValue
    -> Node tBranchValue tLeafValue
replaceNode _ [] newNode = newNode
replaceNode tree (pathSegment:subPath) newNode =
    case tree of
        NodeLeaf leaf ->
            error $
            printf
                "Path prematurely ends at leaf. Leaf: %s. Remaining path: %s"
                (show leaf)
                (show (pathSegment : subPath))
        NodeBranch branch -> NodeBranch $ branch {children = newChildren}
            where subTree = children branch !! pathSegment
                  newSubTree = replaceNode subTree subPath newNode
                  newChildren = replace newSubTree pathSegment (children branch)

--
-- TODO: why is this type signature not broken into multiple lines? -> report hindent issue?
replaceNodeBy ::
       Show tLeafValue
    => Show tPathSegment =>
           Node tBranchValue tLeafValue -> PathPredicate tPathSegment tBranchValue tLeafValue -> [tPathSegment] -> Node tBranchValue tLeafValue -> Node tBranchValue tLeafValue
replaceNodeBy _ _ [] newNode = newNode
replaceNodeBy tree pathPredicate (pathSegment:subPath) newNode =
    case tree of
        NodeLeaf leaf ->
            error $
            printf
                "Path prematurely ends at leaf. Leaf: %s. Remaining path: %s"
                (show leaf)
                (show (pathSegment : subPath))
        NodeBranch branch -> NodeBranch $ branch {children = newChildren}
            where nodePredicate = pathPredicate pathSegment
                  subTree = findChild branch nodePredicate
                  newSubTree = replaceNodeBy subTree pathPredicate subPath newNode
                  newChildren = replaceBy (children branch) nodePredicate newSubTree
