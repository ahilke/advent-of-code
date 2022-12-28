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

import Debug (traceNothing)
import List (replace, replaceBy)

import Data.List (find)
import Data.Maybe (fromJust)
import Text.Printf (printf)

-- TODO: split into core and utility (like findAllNodesBy); split path predicate functions with suffix `by`
-- TODO: add tests for these functions
-- TODO: add pretty print
--
data Branch tBranchValue tLeafValue =
    Branch
        { branchValue :: tBranchValue
        , children :: [Node tBranchValue tLeafValue]
        }
    deriving (Show, Eq)

newtype Leaf tLeafValue =
    Leaf
        { leafValue :: tLeafValue
        }
    deriving (Show, Eq)

data Node tBranchValue tLeafValue
    = NodeBranch (Branch tBranchValue tLeafValue)
    | NodeLeaf (Leaf tLeafValue)
    deriving (Show, Eq)

-- todo: add getNodeValue to Node? https://stackoverflow.com/a/6119257/10380981
-- getNodeValue :: Node a b -> (a|b)
-- getNodeValue (NodeBranch branch) = branchValue branch
-- getNodeValue (NodeLeaf leaf) = leafValue leaf
--
type NodePredicate tBranchValue tLeafValue = Node tBranchValue tLeafValue -> Bool

type PathPredicate tPathSegment tBranchValue tLeafValue = tPathSegment -> NodePredicate tBranchValue tLeafValue

findChild ::
       Branch tBranchValue tLeafValue -> NodePredicate tBranchValue tLeafValue -> Maybe (Node tBranchValue tLeafValue)
findChild branch predicate = find predicate (children branch)

-- todo: do not export this (and possible some other non-needed functions)
appendChild :: Branch tBranchValue tLeafValue -> Node tBranchValue tLeafValue -> Branch tBranchValue tLeafValue
appendChild branch node = branch {children = children branch ++ [node]}

addNodeToTree ::
       Show tLeafValue
    => Node tBranchValue tLeafValue
    -> [Int]
    -> Node tBranchValue tLeafValue
    -> Maybe (Node tBranchValue tLeafValue)
addNodeToTree tree path node = replaceNode tree path (NodeBranch (appendChild branch node))
  where
    nodeAtPath = findNode tree path
    branch =
        fromJust $
        case fromJust nodeAtPath of
            NodeLeaf leafAtPath -> traceNothing $ printf "Cannot append to leaf. Leaf: %s." (show leafAtPath)
            NodeBranch branchAtPath -> Just branchAtPath

addNodeToTreeBy ::
       Show tLeafValue
    => Show tPathSegment =>
           Node tBranchValue tLeafValue -> PathPredicate tPathSegment tBranchValue tLeafValue -> [tPathSegment] -> Node tBranchValue tLeafValue -> Maybe (Node tBranchValue tLeafValue)
addNodeToTreeBy tree pathPredicate path node =
    replaceNodeBy tree pathPredicate path (NodeBranch (appendChild branch node))
  where
    nodeAtPath = findNodeBy tree pathPredicate path
    branch =
        fromJust $
        case fromJust nodeAtPath of
            NodeLeaf leafAtPath -> traceNothing $ printf "Cannot append to leaf. Leaf: %s." (show leafAtPath)
            NodeBranch branchAtPath -> Just branchAtPath

findNode :: Show tLeafValue => Node tBranchValue tLeafValue -> [Int] -> Maybe (Node tBranchValue tLeafValue)
findNode tree [] = Just tree
findNode tree (pathSegment:subPath) =
    case tree of
        NodeLeaf leaf ->
            traceNothing $
            printf
                "Path prematurely ends at leaf. Leaf: %s. Remaining path: %s"
                (show leaf)
                (show (pathSegment : subPath))
        NodeBranch branch -> findNode subTree subPath
            where subTree = children branch !! pathSegment

findNodeBy ::
       Show tLeafValue
    => Show tPathSegment =>
           Node tBranchValue tLeafValue -> PathPredicate tPathSegment tBranchValue tLeafValue -> [tPathSegment] -> Maybe (Node tBranchValue tLeafValue)
findNodeBy tree _ [] = Just tree
findNodeBy tree pathPredicate (pathSegment:subPath) =
    case tree of
        NodeLeaf leaf ->
            traceNothing $
            printf
                "Path prematurely ends at leaf. Leaf: %s. Remaining path: %s"
                (show leaf)
                (show (pathSegment : subPath))
        NodeBranch branch -> findNodeBy subTree pathPredicate subPath
            where nodePredicate = pathPredicate pathSegment
                  subTree = fromJust $ findChild branch nodePredicate

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
    -> Maybe (Node tBranchValue tLeafValue)
replaceNode _ [] newNode = Just newNode
replaceNode tree (pathSegment:subPath) newNode =
    case tree of
        NodeLeaf leaf ->
            traceNothing $
            printf
                "Path prematurely ends at leaf. Leaf: %s. Remaining path: %s"
                (show leaf)
                (show (pathSegment : subPath))
        NodeBranch branch -> Just $ NodeBranch $ branch {children = newChildren}
            where subTree = children branch !! pathSegment
                  newSubTree = fromJust $ replaceNode subTree subPath newNode
                  newChildren = replace newSubTree pathSegment (children branch)

--
-- TODO: why is this type signature not broken into multiple lines? -> report hindent issue?
replaceNodeBy ::
       Show tLeafValue
    => Show tPathSegment =>
           Node tBranchValue tLeafValue -> PathPredicate tPathSegment tBranchValue tLeafValue -> [tPathSegment] -> Node tBranchValue tLeafValue -> Maybe (Node tBranchValue tLeafValue)
replaceNodeBy _ _ [] newNode = Just newNode
replaceNodeBy tree pathPredicate (pathSegment:subPath) newNode =
    case tree of
        NodeLeaf leaf ->
            traceNothing $
            printf
                "Path prematurely ends at leaf. Leaf: %s. Remaining path: %s"
                (show leaf)
                (show (pathSegment : subPath))
        NodeBranch branch -> Just $ NodeBranch $ branch {children = newChildren}
            where nodePredicate = pathPredicate pathSegment
                  subTree = fromJust $ findChild branch nodePredicate
                  newSubTree = fromJust $ replaceNodeBy subTree pathPredicate subPath newNode
                  newChildren = replaceBy (children branch) nodePredicate newSubTree
