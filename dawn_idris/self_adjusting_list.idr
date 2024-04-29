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