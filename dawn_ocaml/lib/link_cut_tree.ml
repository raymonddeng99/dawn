open Base

module LinkCutTree = struct
  type node = {
    value: int;
    parent: node option ref;
    left: node option ref;
    right: node option ref;
    mutable rev: bool;
    mutable sum: int;
  }

  let make_node value =
    {
      value;
      parent = ref None;
      left = ref None;
      right = ref None;
      rev = false;
      sum = value;
    }

  let get_sum node =
    match !node with
    | None -> 0
    | Some n -> n.sum

  let update_sum node =
    match !node with
    | None -> ()
    | Some n ->
        n.sum <-
          n.value + get_sum n.left + get_sum n.right

  let push node =
    match !node with
    | None -> ()
    | Some n ->
        if n.rev then (
          n.rev <- false;
          let temp = n.left in
          n.left <- n.right;
          n.right <- temp;
          Option.iter
            (fun left ->
              left.rev <- not left.rev)
            !n.left;
          Option.iter
            (fun right ->
              right.rev <- not right.rev)
            !n.right)

  let make_root node =
    let rec loop x =
      push x;
      match !x.parent with
      | None -> ()
      | Some y ->
          y.left := None;
          y.right := None;
          x.parent := None;
          loop (Some y)
    in
    loop node

  let splay node =
    let rec zig y =
      match !y.parent with
      | None -> make_root y
      | Some x ->
          if !x.left == Some y then (
            push x;
            push y;
            let p = !x.parent in
            x.parent := Some (!y.parent);
            y.parent := p;
            if p == None then make_root y
            else if Some (!p).left == Some x then
              p.left := Some y
            else p.right := Some y;
            x.left := !y.right;
            y.right := Some x;
            update_sum x;
            update_sum y)
          else zig x
    and zig_zig y =
      match !y.parent with
      | None -> make_root y
      | Some x ->
          zig_zig x;
          zig y
    and zig_zag y =
      let x = Option.get !y.parent in
      push x;
      push y;
      let p = !x.parent in
      let q = !y.right in
      x.parent := Some (!q.parent);
      y.parent := Some (!q.parent);
      q.parent := Some x;
      x.left := Some q;
      y.right := !q.left;
      q.left := Some y;
      if p == None then make_root q
      else if Some (!p).left == Some x then
        p.left := Some q
      else p.right := Some q;
      update_sum x;
      update_sum y;
      update_sum q
    in
    make_root node;
    zig_zag node;
    update_sum node

  let access node =
    splay node;
    let rec loop x rev =
      if !x == node then rev
      else
        (push x;
         let rev' =
           (match !x.left with
            | None -> not rev
            | Some y -> loop y rev)
         in
         x.rev <- rev';
         update_sum x;
         rev')
    in
    ignore (loop node false)

  let link (x, y) =
    access x;
    access y;
    x.parent := Some y;
    update_sum y

  let cut x =
    access x;
    match !x.left with
    | None -> ()
    | Some y ->
        y.parent := None;
        x.left := None;
        update_sum x

  let root x = access x; !x.parent = None

  let lca (x, y) =
    access x;
    let x_sum = get_sum x in
    access y;
    let rec loop z =
      let z_sum = get_sum z in
      if z_sum >= x_sum && z_sum >= get_sum y then z
      else
        (access (Option.get !z.parent);
         loop (Option.get !z.parent))
    in
    loop y

  let path_sum (x, y) =
    let z = lca (x, y) in
    access x;
    access y;
    get_sum x + get_sum y - 2. *. get_sum z
end