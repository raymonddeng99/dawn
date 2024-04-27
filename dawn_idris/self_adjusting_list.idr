module SelfAdjustingList

%default total

data Node : Type -> Type where
  MkNode : (prev : Maybe (Node a)) ->
           (next : Maybe (Node a)) ->
           (data : a) ->
           Node a

data SelfAdjustingList : Type -> Type where
  MkSelfAdjustingList : (head : Maybe (Node a)) ->
                        (tail : Maybe (Node a)) ->
                        (finger : Maybe (Node a)) ->
                        SelfAdjustingList a

create : SelfAdjustingList a
create = MkSelfAdjustingList Nothing Nothing Nothing

insertFront : a -> SelfAdjustingList a -> SelfAdjustingList a
insertFront x (MkSelfAdjustingList h t f) =
  let newNode = MkNode Nothing h x in
    case h of
      Nothing => MkSelfAdjustingList (Just newNode) (Just newNode) (Just newNode)
      Just hd => MkSelfAdjustingList (Just newNode) t (Just newNode)

traverseFromFinger : Eq a => a -> SelfAdjustingList a -> Maybe (Node a) -> Maybe (Node a)
traverseFromFinger x (MkSelfAdjustingList h t f) node =
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
                         MkSelfAdjustingList h t newFinger
                         Nothing
            Just next => case traverseFromFinger x (MkSelfAdjustingList h t f) next of
                           Nothing => let newFinger = node in
                                        MkSelfAdjustingList h t newFinger
                                        node
                           found => found

find : Eq a => a -> SelfAdjustingList a -> Maybe (Node a)
find x (MkSelfAdjustingList h t f) =
  case f of
    Nothing => case h of
                 Nothing => Nothing
                 Just hd => traverseFromFinger x (MkSelfAdjustingList h t f) (Just hd)
    Just finger => case traverseFromFinger x (MkSelfAdjustingList h t f) (Just finger) of
                     Nothing => case h of
                                  Nothing => Nothing
                                  Just hd => traverseFromFinger x (MkSelfAdjustingList h t f) (Just hd)
                     found => found