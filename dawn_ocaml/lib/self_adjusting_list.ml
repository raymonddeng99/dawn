open Base

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