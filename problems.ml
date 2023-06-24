(* Tail of a List *)
(* Beginner difficulty *)
(* Write a function last : 'a list -> 'a option that returns the last element of a list *)
let rec last l =
    match l with
    | [] -> None
    | [el] -> Some el
    | hd :: rest -> last rest
;;

(* Last Two Elements of a List *)
(* Beginner difficulty *)
(* Find the last but one (last and penultimate) elements of a list. *)
let rec last_two l = 
    match l with
    | [] -> None
    | [_] -> None
    | [a ; b] -> Some (a, b)
    | hd :: rest -> last_two rest
;;

(* N'th Element of a List *)
(* Beginner difficulty *)
(* Find the N'th element of a list. *)
let rec nth l n =
    match (l, n) with
    | (hd :: rest, 0) -> hd
    | ([], _) -> raise (Failure "nth")
    | (hd :: rest, _) -> nth rest (n-1)
;;

(* Length of a List *)
(* Beginner difficulty *)
(* Find the number of elements of a list. *)
let rec len l =
    match l with
    | [] -> 0
    | hd :: rest -> len rest + 1
;;

(* Reverse a List *)
(* Beginner difficulty *)
(* Reverse a list. *)
let rec rev l =
    match l with
    | [] -> []
    | hd::rest -> (rev rest) @ [hd]
;;

(* Palindrome *)
(* Peginner difficulty *)
(* Find out whether a list is a palindrome. *)
let is_palindrome l =
    l = rev l
;;

(* Flatten a List *)
(* Intermediate difficulty *)
(* Flatten a nested list structure. *)
type 'a node =
  | One of 'a 
  | Many of 'a node list
;;
let rec flatten l =
    match l with
    | [] -> []
    | (One el)::rest -> el::(flatten rest)
    | (Many el)::rest -> (flatten el) @ (flatten rest)
;;

(* Eliminate Duplicates *)
(* Intermediate difficulty *)
(* Eliminate consecutive duplicates of list elements. *)
let rec contains l el =
    match l with
    | [] -> false
    | hd::rest when hd = el -> true
    | hd::rest -> contains rest el
;;
let rec compress l =
    match l with
    | [] -> []
    | hd::rest ->
            let rest = compress rest in
            if contains rest hd then rest else hd::rest
;;

