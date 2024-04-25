import Data.SortedMap

-- On-line construction of suffix trees, Ukkoken '95

record Node where
  constructor MkNode
  start : Int
  len : Int
  children : SortedMap Char Node
  suffixLink : Maybe Node
  parent : Maybe Node

root : Node
root = MkNode 0 0 empty Nothing Nothing

createNode : Int -> Int -> Maybe Node -> Node
createNode start len parent = MkNode start len empty Nothing parent

splitNode : Node -> Int -> Int -> Char -> Node
splitNode node start len nodeLabel =
  let newNode = createNode start len (parent node)
      remainingLen = len node - len - 1
      child = createNode (start node + len + 1) remainingLen (Just newNode)
  in record { len = len
            , children = insert nodeLabel child (children node)
            , suffixLink = Just newNode } node
       { suffixLink = suffixLink node }

findNode : Node -> Int -> Int -> String -> Maybe Node
findNode root start len str =
  if len == 0
     then Just root
     else case lookup (strIndex str start) (children root) of
            Just child =>
              let childLen = len child in
                  if childLen >= len
                     then Just child
                     else findNode child (start child + childLen + 1) (len - childLen - 1) str
            Nothing => Nothing

followLinks : Node -> Node
followLinks node = case suffixLink node of
  Just link => followLinks link
  Nothing => node

updateTree : String -> Int -> Node -> Node
updateTree str i node =
  let len = length str
      remaining = len - i
  in if remaining == 0
        then node
        else let leaf = createNode i remaining (Just node)
                 nodeLabel = strIndex str i
             in case lookup nodeLabel (children node) of
                   Just child =>
                     let start = start child
                         len' = len child
                         foundNode = findNode root start len' str
                     in case foundNode of
                           Just foundNode =>
                             if substr start len' str == substr i len' str
                                then updateTree str (i + len' + 1) child
                                else let newNode = splitNode foundNode start len' nodeLabel
                                         leaf' = createNode i remaining (Just newNode)
                                     in record { children $= insert nodeLabel leaf' } newNode
                   Nothing => record { children $= insert nodeLabel leaf } node

buildSuffixTree : String -> Node
buildSuffixTree str = foldl (updateTree str) root [0..length str - 1]