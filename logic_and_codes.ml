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
    []
;;
(*
assert (gray 1 = ["0"; "1"]);;
assert (gray 2 = ["00"; "01"; "11"; "10"]);;
assert (gray 3 = ["000"; "001"; "011"; "010"; "110"; "111"; "101"; "100"]);;
*)

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
type 'a node = {
    ch : 'a option;
    fr : int;
    lc : 'a node option;
    rc : 'a node option;
};;

let huffman freqs =
    (* sort list of (char, freq) in ascending order *)
    let sort =
        List.sort
        (fun a b ->
            match (a, b) with
            | ((_, f1), (_, f2)) -> f1 - f2)
    in
    (* transform list of (char, freq) tuples to list of nodes *)
    let rec make_nodes = function
        | [] -> []
        | (ch, fr) :: tl -> { ch = Some ch; fr; lc = None; rc = None } :: make_nodes tl
    in
    (* get first 2 nodes from the list *)
    let next_nodes = function
        | a :: b :: tl -> (a, b)
        | _ -> raise (Failure "unreachable: always at least 2 nodes")
    in
    (* delete first 2 nodes from the list *)
    let delete = function
        | a :: b :: tl -> tl
        | _ -> raise (Failure "unreachable: always delete 2 nodes")
    in
    (* insert node at the appropriate position *)
    let rec insert node list =
        let { fr } = node in
        match list with
        | [] -> [node]
        | { fr=f } as hd :: tl -> if fr < f then node :: list else hd :: insert node tl
    in
    (* make node with child nodes a and b and frequency a.fr + b.fr *)
    let make_node a b =
        match (a, b) with
        | ({ fr=f1 }, { fr=f2 }) -> { ch = None; fr = f1 + f2; lc = Some a; rc = Some b }
    in
    (* build tree *)
    let rec build_tree list =
        if List.length list = 1 then List.hd list
        else
            let (a, b) = next_nodes list in
            list
                |> delete
                |> insert (make_node a b)
                |> build_tree
    in
    (* transform tree to list of huffman codes *)
    let to_huffman nodes =
        let rec aux code = function
            | { ch=Some ch } -> [(ch, code)]
            | { lc=Some lc; rc=Some rc } -> aux (code ^ "0") lc @ aux (code ^ "1") rc
            | _ -> raise (Failure "unreachable: nodes always follow the pattern above")
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

(* My own additions *)

let timeit f_name f arg =
    let open Format in
    let t0 = Sys.time () in
    let _ = f arg in
    let t1 = Sys.time () in
    printf "%f ms <- %s\n" ((t1 -. t0) *. 1000.) f_name;;
;;

(* Encode mesage with huffman *)
let encode message =
    (* calculate frequencies of characters *)
    let freqs string =
        let insert list char =
            let rec aux = function
                | [] -> [(char, 1)]
                | (ch, count) :: tl when ch = char -> (ch, count + 1) :: tl
                | hd :: tl -> hd :: aux tl
            in
            aux list
        in
        String.fold_left insert [] string
    in
    (* encode mesage with huffman *)
    let to_code huffman =
        let add_code string char =
            let rec aux = function
                | (ch, code) :: tl when ch = char -> string ^ code
                | hd :: tl -> aux tl
                | [] -> raise (Failure "unreachable: code for char always exists")
            in
            aux huffman
        in
        String.fold_left add_code "" message
    in
    let _ = timeit "huffman" huffman (freqs message) in

    let huffman = message
                |> freqs
                |> huffman in
    (huffman, to_code huffman)
;;

(* decode huffman encoded message *)
let decode encoded =
    let (huffman, code) = encoded in
    let rec find_char encoded = function
        | (ch, code) :: tl ->
                let code_length = String.length code in
                let length = Int.min code_length (String.length encoded) in
                if code = String.sub encoded 0 length then
                    (ch, code_length)
                else
                    find_char encoded tl
        | [] -> raise (Failure "unreachable: code for char always exists")
    in
    let rec to_message acc = function
        | "" -> acc
        | code ->
                let (char, length) = find_char code huffman in
                let code = String.sub code length (String.length code - length) in
                to_message (acc ^ String.make 1 char) code
    in

    to_message "" code
;;

let message = "
Ogrlico nosila
je moja vila
zato so jo klicali vsi
Maja z biseri,
Maja z biseri.

Obraz kot s španske slike,
oči velike,
nasmehi pa kot čudeži,
Maja z biseri,
Maja z biseri.

Ljudje, ki so jo kdaj poznali,
vzljubili so jo slej ko prej,
da vsak je biser so dejali,
simbol lepote, skrite v njej.

Živeti je začela
in prekipela,
da v nič so se razblinjali
njeni biseri,
njeni biseri.

Srce je neugnano
in razigrano,
ko Maja v sladko noč beži
skupaj z biseri,
skupaj z biseri.

In bolj, ko srečo vsem razdaja,
manj sreče Maja zase ima,
da vedno krajši niz postaja
prepozno, kakor vsak spozna.

Zdaj tisoč milj od raja,
v tančici Maja,
vprašanj neskončnih se boji:
“Kje so biseri,
kje so biseri?”

Ogrlico še eno,
ponarejeno,
je v žalosti kupila si,
Maja z biseri,
Maja z biseri.

A Maja le ni srečna,
ne, ne, ni srečna,
čeprav spet kličejo jo vsi
“Maja z biseri,
Maja z biseri”.

A kaj, ko vsi smo skupaj,
vsi smo kot Maja,
vsaj enkrat smo v življenju vsi
“Maja z biseri,
Maja z biseri…”
";;

let (huffman, code) as encoded = encode message;;

let message_size = String.length message * 8 in
let encoded_size = List.fold_left (fun sum -> function | (ch, code) -> sum + 8 + String.length code) 0 huffman + String.length code in
Format.printf "message size: %d b\nencoded size: %d b\ncompression ratio: %f\n"
    message_size encoded_size (Int.to_float encoded_size /. Int.to_float message_size);;

assert (decode encoded = message);;

timeit "encode" encode message;;
timeit "decode" decode encoded;;
