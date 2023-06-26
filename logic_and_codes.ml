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
And (Or (Var "a", Var "b"), And (Var "a", Var "b"));;

(* Define a function, table2 which returns the truth table of a given logical expression in two variables (specified as arguments).
   The return value must be a list of triples containing (value_of_a, value_of_b, value_of_expr). *)
let table2 a b expr =
    let rec eval b1 b2 = function
        | Var v -> if v = a then b1 else b2
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
(* Generalize the previous problem in such a way that the logical expression may contain any number of logical variables. Define table in a way that table variables expr returns the truth table for the expression expr, which contains the logical variables enumerated in variables. *)
let table var_list expr =
    (* integer power *)
    let pow x n =
        let rec aux acc = function
            | 0 -> acc
            | n -> aux (acc * x) (n - 1)
        in
        aux 1 n
    in
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
        aux [] (List.rev vars) (pow 2 (List.length vars) - 1)
            |> List.rev
    in
    let rec eval expr vars states =
        match expr with
        | Not x -> not (eval x vars states)
        | And (x, y) -> eval x vars states && eval y vars states
        | Or  (x, y) -> eval x vars states || eval y vars states
        | Var v as expr ->
                match states with
                | (var, state) :: states when v = var -> state
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

