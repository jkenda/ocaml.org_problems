(* Construct Completely Balanced Binary Trees *)
(* Intermediate difficulty *)

(* Binary Tree *)
(* A binary tree is either empty or it is composed of a root element and two successors, which are binary trees themselves.
In OCaml, one can define a new type binary_tree that carries an arbitrary value of type 'a (thus is polymorphic) at each node. *)
type 'a binary_tree =
  | Empty
  | Node of 'a * 'a binary_tree * 'a binary_tree;;

(* An example of tree carrying char data is: *)
let _example_tree =
    Node ('a', Node ('b', Node ('d', Empty, Empty), Node ('e', Empty, Empty)),
        Node ('c', Empty, Node ('f', Node ('g', Empty, Empty), Empty)));;

(* In OCaml, the strict type discipline guarantees that, if you get a value of type binary_tree,
    then it must have been created with the two constructors Empty and Node. *)
(* In a completely balanced binary tree, the following property holds for every node:
    The number of nodes in its left subtree and the number of nodes in its right subtree are almost equal,
    which means their difference is not greater than one. *)
(* Write a function cbal_tree to construct completely balanced binary trees for a given number of nodes.
   The function should generate all solutions via backtracking.
   Put the letter 'x' as information into all nodes of the tree. *)
let cbal_tree n =
    () (* TODO *)
;;
(*
assert (cbal_tree 4 = [
    Node ('x', Node ('x', Empty, Empty), Node ('x', Node ('x', Empty, Empty), Empty));
    Node ('x', Node ('x', Empty, Empty), Node ('x', Empty, Node ('x', Empty, Empty)));
    Node ('x', Node ('x', Node ('x', Empty, Empty), Empty), Node ('x', Empty, Empty));
    Node ('x', Node ('x', Empty, Node ('x', Empty, Empty)), Node ('x', Empty, Empty))
]);;
*)

(* Symmetric Binary Trees *)
(* Intermediate difficulty *)
(* Let us call a binary tree symmetric if you can draw a vertical line through the root node
   and then the right subtree is the mirror image of the left subtree.
   Write a function is_symmetric to check whether a given binary tree is symmetric. *)
(* Hint: Write a function is_mirror first to check whether one tree is the mirror image of another.
   We are only interested in the structure, not in the contents of the nodes. *)
let is_symmetric tree =
    let rec is_mirror l r =
        match l, r with
        | Empty, Empty -> true
        | Node (_, ll, lr), Node (_, rl, rr) -> is_mirror ll rr && is_mirror lr rl
        | _ -> false
    in
    match tree with
    | Empty -> true
    | Node (_, l, r) -> is_mirror l r
;;

(* Binary Search Trees (Dictionaries) *)
(* Intermediate difficulty *)
(* Construct a binary search tree from a list of integer numbers. *)
let construct list = 
    let rec insert tree new_el =
        match tree with
        | Empty -> Node (new_el, Empty, Empty)
        | Node (el, l, r) ->
                if new_el <= el then Node (el, insert l new_el, r)
                else Node (el, l, insert r new_el)
    in
    List.fold_left insert Empty list
;;
assert (construct [3; 2; 5; 7; 1] =
Node (3, Node (2, Node (1, Empty, Empty), Empty),
 Node (5, Empty, Node (7, Empty, Empty))));;

(* Then use this function to test the solution of the previous problem. *)

assert (is_symmetric (construct [5; 3; 18; 1; 4; 12; 21]));;
assert (not (is_symmetric (construct [3; 2; 5; 7; 4])));;

