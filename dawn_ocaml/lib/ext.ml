open Base

module PriorityQueue = struct
  type 'a t = {
    mutable size : int;
    mutable root : 'a cluster;
  }
  and 'a cluster =
    | Leaf of 'a
    | Node of 'a cluster array

  let cluster_size = 2

  let empty_cluster () = Array.make cluster_size (Leaf None)

  let rec build_cluster xs =
    match xs with
    | [] -> Leaf None
    | x :: xs' ->
        let cluster = empty_cluster () in
        cluster.(0) <- Leaf x;
        let rec loop i = function
          | [] -> ()
          | x :: xs' ->
              cluster.(i) <- Leaf x;
              loop (i + 1) xs'
        in
        loop 1 xs';
        Node cluster

  let empty () = { size = 0; root = Leaf None }

  let is_empty { size; _ } = size = 0

  let rec insert x { size; root } =
    let root' =
      match root with
      | Leaf None -> Leaf x
      | Leaf _ as leaf -> build_cluster [leaf; Leaf x]
      | Node cluster ->
          let rec ins i = function
            | Leaf None as leaf ->
                cluster.(i) <- Leaf x;
                Node cluster
            | Leaf _ as leaf ->
                let new_cluster = empty_cluster () in
                new_cluster.(0) <- leaf;
                new_cluster.(1) <- Leaf x;
                Node (Array.map (fun c -> build_cluster [c]) cluster)
            | Node cluster' ->
                let new_cluster' = Array.copy cluster' in
                new_cluster'.(i) <- ins 0 cluster'.(i);
                Node new_cluster'
          in
          ins 0 root
    in
    { size = size + 1; root = root' }

  let rec find_min = function
    | Leaf x -> x
    | Node cluster -> find_min (Array.get cluster 0)

  let rec rem_min { size; root } =
    match root with
    | Leaf None -> { size = 0; root = Leaf None }
    | Leaf x -> { size = 0; root = Leaf None }
    | Node cluster ->
        let rec rem i = function
          | Leaf None as leaf -> Leaf None
          | Leaf _ as leaf ->
              let new_cluster = empty_cluster () in
              new_cluster.(0) <- leaf;
              Node new_cluster
          | Node cluster' ->
              let new_cluster' = Array.copy cluster' in
              new_cluster'.(i) <- rem 0 cluster'.(i);
              Node new_cluster'
        in
        let root' = rem 0 root in
        { size = size - 1; root = root' }
end