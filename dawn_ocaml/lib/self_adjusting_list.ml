open Base

(* Sleator-Tarjan one-finger model *)
module SelfAdjustingList = struct
  type 'a node = {
    mutable prev : 'a node option;
    mutable next : 'a node option;
    mutable data : 'a;
  }

  type 'a t = {
    mutable head : 'a node option;
    mutable tail : 'a node option;
    mutable finger : 'a node option;
  }

  let create () = { head = None; tail = None; finger = None }

  let create_node data = { prev = None; next = None; data }

  let insert_front list data =
    let new_node = create_node data in
    begin match list.head with
    | None ->
        list.head <- Some new_node;
        list.tail <- Some new_node;
        list.finger <- Some new_node
    | Some head_node ->
        head_node.prev <- Some new_node;
        new_node.next <- Some head_node;
        list.head <- Some new_node;
        list.finger <- Some new_node
    end

  let rec traverse_from_finger list node target =
    if node.data = target then node
    else begin
      match node.next with
      | None -> list.finger <- list.tail; None
      | Some next_node ->
          let cost_next = traverse_from_finger list next_node target in
          begin match cost_next with
          | None -> list.finger <- Some node; Some node
          | Some found_node -> Some found_node
          end
    end

  let find list target =
    match list.finger with
    | None -> None
    | Some finger_node ->
        let cost_next = traverse_from_finger list finger_node target in
        begin match cost_next with
        | None ->
            list.finger <- list.head;
            begin match list.head with
            | None -> None
            | Some head_node ->
                traverse_from_finger list head_node target
            end
        | Some found_node -> Some found_node
        end
end

(* Constant finger model *)
module ConstList = struct
  type 'a node = {
    value: 'a;
    mutable next: 'a node option;
  }

  type 'a t = {
    mutable head: 'a node option;
    mutable fingers: ('a node option) array;
  }

  let create num_fingers =
    let fingers = Array.make num_fingers None in
    { head = None; fingers }

  let rec find_and_move list x =
    match list.head with
    | None -> None
    | Some head ->
        if head.value = x then (
          list.head <- head.next;
          head.next <- list.head;
          Array.fill list.fingers 0 (Array.length list.fingers) (Some head);
          Some head)
        else
          match find_and_move_helper list.fingers head x with
          | Some node ->
              let finger_idx = ref 0 in
              while !finger_idx < Array.length list.fingers && list.fingers.(!finger_idx) <> Some node do
                incr finger_idx
              done;
              if !finger_idx < Array.length list.fingers then
                list.fingers.(!finger_idx) <- node.next;
              Some node
          | None -> None

  and find_and_move_helper fingers node x =
    match node.next with
    | None -> None
    | Some next ->
        if next.value = x then (
          node.next <- next.next;
          next.next <- list.head;
          Array.fill fingers 0 (Array.length fingers) (Some next);
          list.head <- Some next;
          Some next)
        else
          match find_and_move_helper fingers next x with
          | Some found_node -> Some found_node
          | None ->
              let finger_idx = ref 0 in
              while !finger_idx < Array.length fingers && fingers.(!finger_idx) <> Some node do
                incr finger_idx
              done;
              if !finger_idx < Array.length fingers then
                fingers.(!finger_idx) <- node.next;
              find_and_move_helper fingers next x

  let find list x =
    match find_and_move list x with
    | Some node -> Some node.value
    | None -> None

  let add list x =
    let new_node = { value = x; next = list.head } in
    list.head <- Some new_node;
    Array.fill list.fingers 0 (Array.length list.fingers) (Some new_node)
end

(* Order by Next Request strategy *)
module OBNRList = struct
  type 'a node = {
    mutable value : 'a;
    mutable next : 'a node option;
    mutable prev : 'a node option;
  }

  type 'a list = {
    mutable head : 'a node option;
    mutable tail : 'a node option;
  }

  let create () = { head = None; tail = None }

  let insert list value =
    let new_node = { value; next = None; prev = None } in
    match list.head with
    | None ->
      list.head <- Some new_node;
      list.tail <- Some new_node
    | Some head ->
      new_node.next <- list.head;
      head.prev <- Some new_node;
      list.head <- Some new_node

  let remove_head list =
    match list.head with
    | None -> None
    | Some node ->
      let value = node.value in
      list.head <- node.next;
      (match node.next with
       | None -> list.tail <- None
       | Some next -> next.prev <- None);
      Some value

  let access list value =
    let rec find_node node =
      match node with
      | None -> None
      | Some { value = v; next; prev } ->
        if v = value then
          (match prev with
           | None -> ()
           | Some prev ->
             prev.next <- next;
             (match next with
              | None -> ()
              | Some next -> next.prev <- Some prev))
        else
          find_node next
    in
    let rec move_to_front node =
      match node with
      | None -> ()
      | Some node ->
        (match node.prev with
         | None -> ()
         | Some prev ->
           prev.next <- node.next;
           (match node.next with
            | None -> list.tail <- Some prev
            | Some next -> next.prev <- Some prev);
           node.prev <- None;
           node.next <- list.head;
           (match list.head with
            | None -> list.tail <- Some node
            | Some head -> head.prev <- Some node);
           list.head <- Some node)
    in
    match find_node list.head with
    | None -> ()
    | Some node -> move_to_front (Some node)
end