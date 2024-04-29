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