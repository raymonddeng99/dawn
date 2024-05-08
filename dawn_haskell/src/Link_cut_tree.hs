module LinkCutTree (
    Node(..),
    makeNode,
    getSum,
    updateSum,
    push,
    makeRoot,
    splay,
    access,
    link,
    cut,
    root,
    lca,
    pathSum
) where

import Data.Maybe (isNothing, fromMaybe)

data Node = Node {
    value :: Int,
    parent :: Maybe Node,
    left :: Maybe Node,
    right :: Maybe Node,
    rev :: Bool,
    sum :: Int
} deriving (Show)

makeNode :: Int -> Node
makeNode value = Node {
    value = value,
    parent = Nothing,
    left = Nothing,
    right = Nothing,
    rev = False,
    sum = value
}

getSum :: Maybe Node -> Int
getSum Nothing = 0
getSum (Just node) = sum node

updateSum :: Node -> IO ()
updateSum node = do
    let leftSum = getSum (left node)
    let rightSum = getSum (right node)
    let newSum = value node + leftSum + rightSum
    pure Node { node | sum = newSum }

push :: Node -> IO ()
push node
    | rev node = do
        node.rev <- False
        swapChildren node
        mapM_ pushChild (left node)
        mapM_ pushChild (right node)
    | otherwise = pure ()
    where
        swapChildren node = do
            temp <- left node
            node.left <- right node
            node.right <- temp
        pushChild child = do
            child.rev <- not (rev child)
            push child

makeRoot :: Node -> IO ()
makeRoot node = do
    push node
    case parent node of
        Nothing -> pure ()
        Just parent' -> do
            parent'.left <- Nothing
            parent'.right <- Nothing
            node.parent <- Nothing
            makeRoot parent'

splay :: Node -> IO ()
splay node = do
    makeRoot node
    zigZag node
    updateSum node
    where
        zig node
            | isNothing (parent node) = makeRoot node
            | otherwise = case (parent node, left parent', node == left') of
                (Just parent', True, True) -> do
                    push parent'
                    push node
                    setParent node (parent (parent node))
                    setLeft parent' (right node)
                    setRight node (Just parent')
                    updateSum parent'
                    updateSum node
                    zig parent'
                (_, False, _) -> zig (fromMaybe undefined (parent node))
            where
                left' = left node == Just node
                parent' = fromMaybe undefined (parent node)
        zigZag node = do
            push (fromMaybe undefined (parent node))
            push node
            let q = fromMybe undefined (right node)
            setParent node (parent (parent node))
            setParent q (parent node)
            let p = parent node
            setParent (fromMaybe undefined (parent node)) (Just q)
            setLeft (fromMaybe undefined (parent node)) (Just node)
            setRight node (left q)
            setLeft q (Just node)
            updateSum node
            updateSum (fromMaybe undefined (parent node))
            updateSum q

access :: Node -> IO ()
access node = do
    splay node
    let rec node' rev = do
        push node'
        case left node' of
            Nothing -> do
                node'.rev <- rev
                updateSum node'
                pure rev
            Just child -> do
                newRev <- rec child (not rev)
                node'.rev <- newRev
                updateSum node'
                pure newRev
    rec node False

link :: (Node, Node) -> IO ()
link (x, y) = do
    access x
    access y
    y.parent <- Just x
    updateSum y

cut :: Node -> IO ()
cut node = do
    access node
    case left node of
        Nothing -> pure ()
        Just child -> do
            child.parent <- Nothing
            node.left <- Nothing
            updateSum node

root :: Node -> Bool
root node = isNothing (parent node)

lca :: (Node, Node) -> IO Node
lca (x, y) = do
    access x
    xSum <- getSum (Just x)
    access y
    let rec z
        | zSum >= xSum && zSum >= getSum (Just y) = pure z
        | otherwise = do
            access (fromMaybe undefined (parent z))
            rec (fromMaybe undefined (parent z))
        where
            zSum = getSum (Just z)
    rec y

pathSum :: (Node, Node) -> IO Int
pathSum (x, y) = do
    z <- lca (x, y)
    access x
    access y
    xSum <- getSum (Just x)
    ySum <- getSum (Just y)
    zSum <- getSum (Just z)
    pure (xSum + ySum - 2 * zSum)

setParent :: Node -> Maybe Node -> IO ()
setParent node parent' = pure Node { node | parent = parent' }

setLeft :: Node -> Maybe Node -> IO ()
setLeft node left' = pure Node { node | left = left' }

setRight :: Node -> Maybe Node -> IO ()
setRight node right' = pure Node { node | right = right' }