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
let rec cbal_tree n =
    let aux left right all =
        let add_right_tree all l =
            List.fold_left (fun a r -> Node ('x', l, r) :: a) all right in
        List.fold_left add_right_tree all left
    in

    if n = 0 then [Empty]
    else if n mod 2 = 1 then
        let t = cbal_tree (n / 2) in
        aux t t []
    else (* n even: n-1 nodes for the left & right subtrees altogether. *)
        let t1 = cbal_tree (n / 2 - 1) in
        let t2 = cbal_tree (n / 2) in
        aux t1 t2 (aux t2 t1 [])
;;
assert (cbal_tree 4 = [
    Node ('x', Node ('x', Empty, Empty), Node ('x', Node ('x', Empty, Empty), Empty));
    Node ('x', Node ('x', Empty, Empty), Node ('x', Empty, Node ('x', Empty, Empty)));
    Node ('x', Node ('x', Node ('x', Empty, Empty), Empty), Node ('x', Empty, Empty));
    Node ('x', Node ('x', Empty, Node ('x', Empty, Empty)), Node ('x', Empty, Empty))
]);;

(* Symmetric Binary Trees *)
(* Intermediate difficulty *)
(* Let us call a binary tree symmetric if you can draw a vertical line through the root node
   and then the right subtree is the mirror image of the left subtree.
   Write a function is_symmetric to check whether a given binary tree is symmetric. *)
(* Hint: Write a function is_mirror first to check whether one tree is the mirror image of another.
   We are only interested in the structure, not in the contents of the nodes. *)
let is_symmetric tree =
    let rec is_mirror = function
        | Empty, Empty -> true
        | Node (_, ll, lr), Node (_, rl, rr) -> is_mirror (ll, rr) && is_mirror (lr, rl)
        | _ -> false
    in
    match tree with
    | Empty -> true
    | Node (_, l, r) -> is_mirror (l, r)
;;

(* Binary Search Trees (Dictionaries) *)
(* Intermediate difficulty *)
(* Construct a binary search tree from a list of integer numbers. *)
let construct list = 
    let insert tree new_el =
        let rec aux = function
            | Empty -> Node (new_el, Empty, Empty)
            | Node (el, l, r) ->
                    if new_el <= el then Node (el, aux l, r)
                    else Node (el, l, aux r)
        in aux tree
    in
    List.fold_left insert Empty list
;;
assert (construct [3; 2; 5; 7; 1] =
Node (3, Node (2, Node (1, Empty, Empty), Empty),
 Node (5, Empty, Node (7, Empty, Empty))));;

(* Then use this function to test the solution of the previous problem. *)

assert (is_symmetric (construct [5; 3; 18; 1; 4; 12; 21]));;
assert (not (is_symmetric (construct [3; 2; 5; 7; 4])));;

(* Generate-and-Test Paradigm *)
(* Intermediate difficulty *)
(* Apply the generate-and-test paradigm to construct all symmetric, completely balanced binary trees with a given number of nodes. *)
let sym_cbal_trees n =
    List.filter is_symmetric (cbal_tree n)
;;
assert (sym_cbal_trees 5 = [Node ('x', Node ('x', Node ('x', Empty, Empty), Empty),
  Node ('x', Empty, Node ('x', Empty, Empty)));
 Node ('x', Node ('x', Empty, Node ('x', Empty, Empty)),
  Node ('x', Node ('x', Empty, Empty), Empty))]);;
assert (List.length (sym_cbal_trees 57) = 256);;

(* Construct Height-Balanced Binary Trees *)
(* Intermediate difficulty *)
(* In a height-balanced binary tree, the following property holds for every node:
    The height of its left subtree and the height of its right subtree are almost equal,
    which means their difference is not greater than one. *)
(* Write a function hbal_tree to construct height-balanced binary trees for a given height.
   The function should generate all solutions via backtracking.
   Put the letter 'x' as information into all nodes of the tree. *)
let hbal_tree h =
    ()
;;

(* Count the Leaves of a Binary Tree *)
(* A leaf is a node with no successors. Write a function count_leaves to count them. *)
let rec count_leaves = function
    | Empty -> 0
    | Node (_, Empty, Empty) -> 1
    | Node (_, l, r) -> count_leaves l + count_leaves r
;;
assert (count_leaves Empty = 0);;
assert (count_leaves (Node ('x', Node ('y', Empty, Empty), Empty)) = 1);;
assert (count_leaves (Node (1, Node (2, Empty, Empty), Node (3, Empty, Empty))) = 2);;

(* Collect the Leaves of a Binary Tree in a List *)
(* Beginner difficulty *)
(* A leaf is a node with no successors. Write a function leaves to collect them in a list. *)
let rec leaves = function
    | Empty -> []
    | Node (a, Empty, Empty) -> [a]
    | Node (_, l, r) -> leaves l @ leaves r
;;
assert (leaves Empty = []);;
assert (leaves (Node ('x', Node ('y', Empty, Empty), Empty)) = ['y']);;
assert (leaves (Node (1, Node (2, Empty, Empty), Node (3, Empty, Empty))) = [2; 3]);;

(* Collect the Internal Nodes of a Binary Tree in a List *)
(* Beginner difficulty *)
(* An internal node of a binary tree has either one or two non-empty successors.
   Write a function internals to collect them in a list. *)
let rec internals = function
    | Empty
    | Node (_, Empty, Empty) -> []
    | Node (a, l, r) -> a :: internals l @ internals r
;;
assert (internals (Node ('a', Empty, Empty)) = []);;
assert (internals (Node ('x', Node ('y', Empty, Empty), Empty)) = ['x']);;
assert (internals (Node (1, Node (2, Empty, Empty), Node (3, Empty, Empty))) = [1]);;

(* Collect the Nodes at a Given Level in a List *)
(* Beginner difficulty *)
(* A node of a binary tree is at level N if the path from the root to the node has length N-1.
   The root node is at level 1.
   Write a function at_level t l to collect all nodes of the tree t at level l in a list. *)
let rec at_level tree n =
    match n, tree with
    | _, Empty -> []
    | 1, Node (a, _, _) -> [a]
    | n, Node (_, l, r) -> at_level l (n - 1) @ at_level r (n - 1)
;;
let example_tree =
    Node ('a', Node ('b', Node ('d', Empty, Empty), Node ('e', Empty, Empty)),
       Node ('c', Empty, Node ('f', Node ('g', Empty, Empty), Empty)));;
assert (at_level example_tree 2 = ['b'; 'c']);;

(* Construct a Complete Binary Tree *)
(* Intermediate difficulty *)
(* A complete binary tree with height H is defined as follows:
    The levels 1,2,3,...,H-1 contain the maximum number of nodes (i.e 2i-1 at the level i,
    note that we start counting the levels from 1 at the root). In level H,
    which may contain less than the maximum possible number of nodes, all the nodes are "left-adjusted".
    This means that in a levelorder tree traversal all internal nodes come first,
    the leaves come second, and empty successors (the nil's which are not really nodes!) come last. *)
(* Particularly, complete binary trees are used as data structures (or addressing schemes) for heaps. *)
(* We can assign an address number to each node in a complete binary tree by enumerating the nodes in levelorder,
   starting at the root with number 1.
   In doing so, we realize that for every node X with address A the following property holds:
       The address of X's left and right successors are 2*A and 2*A+1, respectively, supposed the successors do exist.
   This fact can be used to elegantly construct a complete binary tree structure.
   Write a function is_complete_binary_tree with the following specification:
       is_complete_binary_tree n t returns true iff t is a complete binary tree with n nodes. *)
let complete_binary_tree list =
    let len = List.length list in
    let rec aux = function
        | n when n > len -> Empty
        | n -> Node (List.nth list (n - 1), aux (2 * n), aux (2 * n + 1))
    in
    aux 1
;;
assert (complete_binary_tree [1; 2; 3; 4; 5; 6] =
Node (1, Node (2, Node (4, Empty, Empty), Node (5, Empty, Empty)),
 Node (3, Node (6, Empty, Empty), Empty)))

