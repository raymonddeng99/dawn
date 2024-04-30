module SelfAdjustingList

%default total

data Node : Type -> Type where
  MkNode : (prev : Maybe (Node a)) ->
           (next : Maybe (Node a)) ->
           (data : a) ->
           Node a

data SelfAdjustingList : Type -> Type where
  MkConstList : (head : Maybe (Node a)) ->
                        (tail : Maybe (Node a)) ->
                        (finger : Maybe (Node a)) ->
                        SelfAdjustingList a

create : SelfAdjustingList a
create = MkConstList Nothing Nothing Nothing

insertFront : a -> SelfAdjustingList a -> SelfAdjustingList a
insertFront x (MkConstList h t f) =
  let newNode = MkNode Nothing h x in
    case h of
      Nothing => MkConstList (Just newNode) (Just newNode) (Just newNode)
      Just hd => MkConstList (Just newNode) t (Just newNode)

traverseFromFinger : Eq a => a -> SelfAdjustingList a -> Maybe (Node a) -> Maybe (Node a)
traverseFromFinger x (MkConstList h t f) node =
  let nodeData = case node of
                   Just (MkNode _ _ data) => data
                   Nothing => ?hole
  in
    if nodeData == x
      then node
      else
        let nextNode = case node of
                         Just (MkNode _ next _) => next
                         Nothing => Nothing
        in
          case nextNode of
            Nothing => let newFinger = t in
                         MkConstList h t newFinger
                         Nothing
            Just next => case traverseFromFinger x (MkConstList h t f) next of
                           Nothing => let newFinger = node in
                                        MkConstList h t newFinger
                                        node
                           found => found

find : Eq a => a -> SelfAdjustingList a -> Maybe (Node a)
find x (MkConstList h t f) =
  case f of
    Nothing => case h of
                 Nothing => Nothing
                 Just hd => traverseFromFinger x (MkConstList h t f) (Just hd)
    Just finger => case traverseFromFinger x (MkConstList h t f) (Just finger) of
                     Nothing => case h of
                                  Nothing => Nothing
                                  Just hd => traverseFromFinger x (MkConstList h t f) (Just hd)
                     found => found



module ConstList

%default total

data Node : Type -> Type where
  MkNode : (prev : Maybe (Node a)) ->
           (next : Maybe (Node a)) ->
           (data : a) ->
           Node a

data ConstList : Type -> Type where
  MkConstList : (head : Maybe (Node a)) ->
                        (tail : Maybe (Node a)) ->
                        (finger : Maybe (Node a)) ->
                        ConstList a

create : ConstList a
create = MkConstList Nothing Nothing Nothing

insertFront : a -> ConstList a -> ConstList a
insertFront x (MkConstList h t f) =
  let newNode = MkNode Nothing h x in
    case h of
      Nothing => MkConstList (Just newNode) (Just newNode) (Just newNode)
      Just hd => MkConstList (Just newNode) t (Just newNode)

traverseFromFinger : Eq a => a -> ConstList a -> Maybe (Node a) -> Maybe (Node a)
traverseFromFinger x (MkConstList h t f) node =
  let nodeData = case node of
                   Just (MkNode _ _ data) => data
                   Nothing => ?hole
  in
    if nodeData == x
      then node
      else
        let nextNode = case node of
                         Just (MkNode _ next _) => next
                         Nothing => Nothing
        in
          case nextNode of
            Nothing => let newFinger = t in
                         MkConstList h t newFinger
                         Nothing
            Just next => case traverseFromFinger x (MkConstList h t f) next of
                           Nothing => let newFinger = node in
                                        MkConstList h t newFinger
                                        node
                           found => found

find : Eq a => a -> ConstList a -> Maybe (Node a)
find x (MkConstList h t f) =
  case f of
    Nothing => case h of
                 Nothing => Nothing
                 Just hd => traverseFromFinger x (MkConstList h t f) (Just hd)
    Just finger => case traverseFromFinger x (MkConstList h t f) (Just finger) of
                     Nothing => case h of
                                  Nothing => Nothing
                                  Just hd => traverseFromFinger x (MkConstList h t f) (Just hd)
                     found => found


-- Order by Next Request strategy
module OBNRList

data Node : Type -> Type where
  MkNode : (value : a) -> (next : Maybe (Node a)) -> (prev : Maybe (Node a)) -> Node a

data SelfAdjustingList : Type -> Type where
  MkSelfAdjustingList : (head : Maybe (Node a)) -> (tail : Maybe (Node a)) -> SelfAdjustingList a

empty : SelfAdjustingList a
empty = MkSelfAdjustingList Nothing Nothing

insert : a -> SelfAdjustingList a -> SelfAdjustingList a
insert val (MkSelfAdjustingList head tail) =
  let newNode = MkNode val head Nothing in
  case head of
    Nothing => MkSelfAdjustingList (Just newNode) (Just newNode)
    Just h => let h' = record { prev = Just newNode } h in
              MkSelfAdjustingList (Just newNode) tail

removeHead : SelfAdjustingList a -> Maybe (a, SelfAdjustingList a)
removeHead (MkSelfAdjustingList Nothing _) = Nothing
removeHead (MkSelfAdjustingList (Just h) t) =
  let updateTail : Maybe (Node a) -> Maybe (Node a)
      updateTail Nothing = Nothing
      updateTail (Just n) = Just (record { prev = Nothing } n) in
  Just (value h, MkSelfAdjustingList (next h) (updateTail t))

access : Eq a => a -> SelfAdjustingList a -> SelfAdjustingList