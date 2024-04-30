-- Sleator-Tarjan one-finger model
module SelfAdjustingList
  ( SelfAdjustingList
  , create
  , insertFront
  , find
  ) where

data Node a = Node { prev :: Maybe (Node a)
                   , next :: Maybe (Node a)
                   , data :: a
                   }

data SelfAdjustingList a = SelfAdjustingList { head :: Maybe (Node a)
                                             , tail :: Maybe (Node a)
                                             , finger :: Maybe (Node a)
                                             }

create :: SelfAdjustingList a
create = SelfAdjustingList Nothing Nothing Nothing

insertFront :: a -> SelfAdjustingList a -> SelfAdjustingList a
insertFront x list@(SelfAdjustingList h t f) =
  let newNode = Node Nothing h x
  in case h of
    Nothing -> SelfAdjustingList (Just newNode) (Just newNode) (Just newNode)
    Just hd -> SelfAdjustingList (Just newNode) t (Just newNode)

traverseFromFinger :: Eq a => a -> SelfAdjustingList a -> Maybe (Node a) -> Maybe (Node a)
traverseFromFinger x list@(SelfAdjustingList h t f) node =
  if data node == x
    then Just node
    else case next node of
      Nothing -> let newFinger = t
                 in SelfAdjustingList h t newFinger >> Nothing
      Just next' -> case traverseFromFinger x list next' of
        Nothing -> let newFinger = Just node
                   in SelfAdjustingList h t newFinger >> Just node
        found -> found

find :: Eq a => a -> SelfAdjustingList a -> Maybe (Node a)
find x list@(SelfAdjustingList h t f) =
  case f of
    Nothing -> case h of
      Nothing -> Nothing
      Just hd -> traverseFromFinger x list hd
    Just finger -> case traverseFromFinger x list finger of
      Nothing -> case h of
        Nothing -> Nothing
        Just hd -> traverseFromFinger x list hd
      found -> found

-- Constant finger model
module ConstList
  ( SelfAdjustingList
  , create
  , insertFront
  , find
  ) where

data Node a = Node { prev :: Maybe (Node a)
                   , next :: Maybe (Node a)
                   , data :: a
                   }

data SelfAdjustingList a = SelfAdjustingList { head :: Maybe (Node a)
                                             , tail :: Maybe (Node a)
                                             , finger :: Maybe (Node a)
                                             }

create :: SelfAdjustingList a
create = SelfAdjustingList Nothing Nothing Nothing

insertFront :: a -> SelfAdjustingList a -> SelfAdjustingList a
insertFront x list@(SelfAdjustingList h t f) =
  let newNode = Node Nothing h x
  in case h of
    Nothing -> SelfAdjustingList (Just newNode) (Just newNode) (Just newNode)
    Just hd -> SelfAdjustingList (Just newNode) t (Just newNode)

traverseFromFinger :: Eq a => a -> SelfAdjustingList a -> Maybe (Node a) -> Maybe (Node a)
traverseFromFinger x list@(SelfAdjustingList h t f) node =
  if data node == x
    then Just node
    else case next node of
      Nothing -> let newFinger = t
                 in SelfAdjustingList h t newFinger >> Nothing
      Just next' -> case traverseFromFinger x list next' of
        Nothing -> let newFinger = Just node
                   in SelfAdjustingList h t newFinger >> Just node
        found -> found

find :: Eq a => a -> SelfAdjustingList a -> Maybe (Node a)
find x list@(SelfAdjustingList h t f) =
  case f of
    Nothing -> case h of
      Nothing -> Nothing
      Just hd -> traverseFromFinger x list hd
    Just finger -> case traverseFromFinger x list finger of
      Nothing -> case h of
        Nothing -> Nothing
        Just hd -> traverseFromFinger x list hd
      found -> found


-- Order by Next Request strategy
module OBNRList (SelfAdjustingList, empty, insert, removeHead, access) where

data Node a = Node { value :: a, next :: Maybe (Node a), prev :: Maybe (Node a) } deriving (Show)
data SelfAdjustingList a = SelfAdjustingList { head :: Maybe (Node a), tail :: Maybe (Node a) } deriving (Show)

empty :: SelfAdjustingList a
empty = SelfAdjustingList Nothing Nothing

insert :: a -> SelfAdjustingList a -> SelfAdjustingList a
insert val list = SelfAdjustingList (Just newNode) (case head (list) of
                                                     Nothing -> Just newNode
                                                     Just h -> tail list)
  where newNode = Node val (head list) Nothing
        updatePrev (Just n) = n { prev = Just newNode }
        updatePrev Nothing = Nothing

removeHead :: SelfAdjustingList a -> Maybe (a, SelfAdjustingList a)
removeHead (SelfAdjustingList Nothing _) = Nothing
removeHead (SelfAdjustingList (Just h) t) = Just (value h, SelfAdjustingList (next h) (updateTail t))
  where updateTail Nothing = Nothing
        updateTail (Just n) = Just (n { prev = Nothing })

access :: Eq a => a -> SelfAdjustingList a -> SelfAdjustingList a
access val list = moveToFront $ findNode val list
  where findNode val (SelfAdjustingList Nothing _) = Nothing
        findNode val (SelfAdjustingList (Just h) t) =
          if value h == val
            then Just h
            else findNode val (SelfAdjustingList (next h) t)
        moveToFront Nothing = list
        moveToFront (Just n) = SelfAdjustingList (Just n) (updateTail list)
          where updateTail (SelfAdjustingList _ Nothing) = tail list
                updateTail (SelfAdjustingList h (Just t)) = SelfAdjustingList (next n) (Just t)
                updatePrev (Just p) = p { next = next n }
                updatePrev Nothing = Nothing
                updateNext (Just n') = n' { prev = prev n }
                updateNext Nothing = Nothing
                (Just h') = head list
                h' { prev = Just n } = h'
                n { next = Just h', prev = Nothing } = n