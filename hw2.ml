(*
  William Ee
  CS 496-A
  Due 2/23/2025
  I pledge my honor that I have abided by the Stevens Honor System.
*)

type dTree =
  | Leaf of int
  | Node of char * dTree * dTree

let tLeft =
  Node ('w', Node ('x', Leaf 2, Leaf 5), Leaf 8)

let tRight =
  Node ('w', Node ('y', Leaf 5, Leaf 7), Node ('x', Leaf 5, Leaf 2))

let rec height = function
  | Leaf _ -> 1
  | Node (_, left, right) -> 1 + max (height left) (height right)  (* Height = max of left/right subtrees + 1 *)

let rec size = function
  | Leaf _ -> 1
  | Node (_, left, right) -> 1 + size left + size right  (* Counts root node + sum of subtrees *)

let rec paths = function
  | Leaf _ -> [[]]
  | Node (_, left, right) ->
      (List.map (fun p -> 0 :: p) (paths left)) @  (* Prefixes left paths with 0 *)
      (List.map (fun p -> 1 :: p) (paths right))  (* Prefixes right paths with 1 *)

let rec is_perfect = function
  | Leaf _ -> true
  | Node (_, left, right) ->
      height left = height right && is_perfect left && is_perfect right  (* Checks equal height and subtrees *)

let rec map f g = function
  | Leaf x -> Leaf (g x)  (* Apply g to leaf values *)
  | Node (c, left, right) -> Node (f c, map f g left, map f g right)  (* Apply f to characters *)

let rec list_to_tree = function
  | [] -> Leaf 0  (* Empty list returns a single leaf with 0 *)
  | x :: xl -> Node (x, list_to_tree xl, list_to_tree xl)  (* Creates nodes with char list *)

let rec replace_leaf_at tree map_val =
  let rec f_val path map_val =
    match map_val with
    | [] -> failwith "Invalid path."
    | (p, v) :: rest -> if p = path then v else f_val path rest  (* Finds value matching path *)
  in
  let rec help path = function
    | Leaf _ -> Leaf (f_val path map_val)  (* Replaces leaf value according to map_val *)
    | Node (c, left, right) -> Node (c, help (path @ [0]) left, help (path @ [1]) right)  (* Recurses for the subtrees *)
  in
  help [] tree

let bf_to_dTree (vars, map_val) =
  replace_leaf_at (list_to_tree vars) map_val  (* Creates tree, replaces leaf values *)
