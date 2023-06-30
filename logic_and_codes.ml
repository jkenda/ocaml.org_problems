(* Truth Tables for Logical Expressions (2 Variables) *)
(* Intermediate difficulty *)
(* Let us define a small "language" for boolean expressions containing variables: *)
type bool_expr =
  | Var of string
  | Not of bool_expr
  | And of bool_expr * bool_expr
  | Or of bool_expr * bool_expr
;;

(* A logical expression in two variables can then be written in prefix notation. For example, (a ∨ b) ∧ (a ∧ b) is written: *)
And (Or (Var "a", Var "b"), Not (And (Var "a", Var "b")));;

(* Define a function, table2 which returns the truth table of a given logical expression in two variables (specified as arguments).
   The return value must be a list of triples containing (value_of_a, value_of_b, value_of_expr). *)
let table2 a b expr =
    let rec eval b1 b2 = function
        | Var v -> if v = a then b1 else if v = b then b2 else raise (Failure "unreachable")
        | Not x -> not (eval b1 b2 x)
        | And (x, y) -> eval b1 b2 x && eval b1 b2 y
        | Or  (x, y) -> eval b1 b2 x || eval b1 b2 y
    in
    [
        ( true,  true, eval  true  true expr);
        ( true, false, eval  true false expr);
        (false,  true, eval false  true expr);
        (false, false, eval false false expr)
    ]
;;
assert (table2 "a" "b" (And (Var "a", Or (Var "a", Var "b")))
= [(true, true, true); (true, false, true); (false, true, false); (false, false, false)]);;
assert (table2 "b" "a" (And (Var "a", Or (Var "a", Var "b")))
= [(true, true, true); (true, false, false); (false, true, true); (false, false, false)]);;


(* Truth Tables for Logical Expressions *)
(* Intermediate difficulty *)
(* Generalize the previous problem in such a way that the logical expression may contain any number of logical variables.
   Define table in a way that table variables expr returns the truth table for the expression expr,
   which contains the logical variables enumerated in variables. *)
let table var_list expr =
    (* list if bits of an integer for a variable *)
    let rec bit_list acc vars = function
        | 0 -> if vars = [] then acc else bit_list ((List.hd vars, false) :: acc) (List.tl vars) 0
        | x -> bit_list ((List.hd vars, if x mod 2 = 1 then true else false) :: acc) (List.tl vars) (x / 2)
    in
    (* truth table for vars (every possible combination of booleans) *)
    let truth_table vars =
        let rec aux acc vars = function
            | -1 -> acc
            | x -> aux (bit_list [] vars x :: acc) vars (x - 1)
        in
        aux [] (List.rev vars) (Int.shift_left 1 (List.length vars) - 1)
            |> List.rev
    in
    let rec eval expr vars states =
        match expr with
        | Not x -> not (eval x vars states)
        | And (x, y) -> eval x vars states && eval y vars states
        | Or  (x, y) -> eval x vars states || eval y vars states
        | Var v as expr ->
                match states with
                | (var, state) :: _ when v = var -> state
                | _ :: states -> eval expr vars states
                | _ -> raise (Failure "invalid state")
    in
    let table = truth_table var_list in

    List.combine table (List.map (eval expr var_list) table)
;;
assert (table ["a"; "b"] (And (Var "a", Or (Var "a", Var "b")))
= [([("a", true); ("b", true)], true); ([("a", true); ("b", false)], true);
   ([("a", false); ("b", true)], false); ([("a", false); ("b", false)], false)]);;
assert (table ["b"; "a"] (And (Var "a", Or (Var "a", Var "b")))
= [([("b", true); ("a", true)], true); ([("b", true); ("a", false)], false);
   ([("b", false); ("a", true)], true); ([("b", false); ("a", false)], false)]);;

(* Gray Code *)
(* Intermediate difficulty *)
(* An n-bit Gray code is a sequence of n-bit strings constructed according to certain rules.
   For example, *)
(*
   n = 1: C(1) = ['0', '1'].
   n = 2: C(2) = ['00', '01', '11', '10'].
   n = 3: C(3) = ['000', '001', '011', '010', '110', '111', '101', '100'].
*)
(* Find out the construction rules and write a function with the following specification:
    gray n returns the n-bit Gray code. *)
let gray n =
    let rec next_level k l =
        if k < n then
            let (first_half, second_half) =
                List.fold_left (fun (acc1, acc2) x ->
                    (("0" ^ x) :: acc1, ("1" ^ x) :: acc2)) ([], []) l
            in
            next_level (k + 1) (List.rev_append first_half second_half)
        else l
    in
    next_level 1 ["0"; "1"]
;;
assert (gray 1 = ["0"; "1"]);;
assert (gray 2 = ["00"; "01"; "11"; "10"]);;
assert (gray 3 = ["000"; "001"; "011"; "010"; "110"; "111"; "101"; "100"]);;


(* Huffman Code *)
(* Advanced difficulty *)
(* First of all, consult a good book on discrete mathematics or algorithms for a detailed description of Huffman codes
   (you can start with the Wikipedia page)! *)
(* We consider a set of symbols with their frequencies.
   For example, if the alphabet is "a",..., "f" (represented as the positions 0,...5)
   and respective frequencies are 45, 13, 12, 16, 9, 5: *)
let fs = [("a", 45); ("b", 13); ("c", 12); ("d", 16);
          ("e", 9); ("f", 5)];;

(* Our objective is to construct the Huffman code c word for all symbols s.
   In our example, the result could be hs
   = [("a", "0"); ("b", "101"); ("c", "100"); ("d", "111"); ("e", "1101"); ("f", "1100")] (or hs = [("a", "1");...]).
   The task shall be performed by the function huffman defined as follows:
       huffman(fs) returns the Huffman code table for the frequency table fs *)
type 'a node =
    | Leaf of int * 'a
    | Node of int * 'a node * 'a node
;;
let freq = function
    | Leaf (fr, _)
    | Node (fr, _, _) -> fr
;;

let huffman freqs =
    (* sort list of (char, freq) in ascending order *)
    let sort =
        List.sort
        (fun (_, f1) (_, f2) -> f1 - f2)
    in
    (* transform list of (char, freq) tuples to list of nodes *)
    let rec make_nodes = function
        | [] -> []
        | (ch, fr) :: tl -> Leaf (fr, ch) :: make_nodes tl
    in
    (* build tree *)
    let rec build_tree list =
        (* make node from first two nodes in the list *)
        let combine = function
            | a :: b :: tl -> Node (freq a + freq b, a, b), tl
            | _ -> raise (Failure "unreachable: always at least 2 nodes")
        in
        (* insert node at the appropriate position *)
        let rec insert (node, list) =
            match list with
            | [] -> [node]
            | hd :: _ as ls when freq node < freq hd -> node :: ls
            | hd :: tl -> hd :: insert (node, tl)
        in

        if List.length list = 1 then List.hd list
        else
            list
                |> combine
                |> insert
                |> build_tree
    in
    (* transform tree to list of huffman codes *)
    let to_huffman nodes =
        let rec aux code = function
            | Leaf (_, ch) -> [(ch, code)]
            | Node (_, lc, rc) -> aux (code ^ "0") lc @ aux (code ^ "1") rc
        in
        aux "" nodes
    in

    freqs
        |> sort
        |> make_nodes
        |> build_tree
        |> to_huffman
;;
assert (huffman fs
= [("a", "0"); ("c", "100"); ("b", "101"); ("f", "1100"); ("e", "1101"); ("d", "111")]);;

