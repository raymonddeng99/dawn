import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (isJust, fromJust)

-- On-line construction of suffix trees, Ukkoken '95

data Node = Node
    { start      :: Int
    , len        :: Int
    , children   :: Map Char Node
    , suffixLink :: Maybe Node
    , parent     :: Maybe Node
    }

type SuffixTree = Node

createNode :: Int -> Int -> Node -> Node
createNode s l p =
    Node s l Map.empty Nothing (Just p)

createRoot :: SuffixTree
createRoot =
    Node 0 0 Map.empty Nothing Nothing

splitNode :: Node -> Int -> Int -> Char -> Node
splitNode parentNode offset len ch =
    let newNode = createNode offset len parentNode
        childNode = createNode (start parentNode + len + 1) (len parentNode - len - 1) newNode
    in newNode
        { children = Map.insert ch childNode (children newNode)
        , suffixLink = suffixLink parentNode
        }

addChild :: Node -> Char -> Node -> Node
addChild parentNode ch childNode =
    parentNode
        { children = Map.insert ch childNode (children parentNode) 
        }

findChild :: Node -> Char -> Maybe Node
findChild parentNode ch =
    Map.lookup ch (children parentNode)

updateSuffixTree :: SuffixTree -> String -> Int -> SuffixTree
updateSuffixTree root s i =
    let n = length s
        leaf = createNode i (n - i) root
        char = s !! i
    in case findChild root char of
        Just existingChild -> 
            if take (len existingChild) s == take (len existingChild) (drop i s)
            then updateSuffixTree existingChild s (i + len existingChild + 1)
            else
                let newNode = splitNode existingChild i (len existingChild - 1) char
                in addChild root char (addChild newNode char leaf)
        Nothing -> addChild root char leaf

buildSuffixTree :: String -> SuffixTree
buildSuffixTree s =
    let root = createRoot
    in foldl (\tree i -> updateSuffixTree tree s i) root [0 .. length s - 1]