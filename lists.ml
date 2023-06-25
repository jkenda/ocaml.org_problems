(* Tail of a List *)
(* Beginner difficulty *)
(* Write a function last : 'a list -> 'a option that returns the last element of a list *)
let rec last l =
    match l with
    | [] -> None
    | [el] -> Some el
    | hd :: tl -> last tl
;;

(* Last Two Elements of a List *)
(* Beginner difficulty *)
(* Find the last but one (last and penultimate) elements of a list. *)
let rec last_two l = 
    match l with
    | [] | [_] -> None
    | [a ; b] -> Some (a, b)
    | hd :: tl -> last_two tl
;;

(* N'th Element of a List *)
(* Beginner difficulty *)
(* Find the N'th element of a list. *)
let rec nth l n =
    match (l, n) with
    | ([], _) -> raise (Failure "nth")
    | (hd :: tl, 0) -> hd
    | (hd :: tl, _) -> nth tl (n-1)
;;

(* Length of a List *)
(* Beginner difficulty *)
(* Find the number of elements of a list. *)
let rec len l =
    match l with
    | [] -> 0
    | hd :: tl -> len tl + 1
;;

(* Reverse a List *)
(* Beginner difficulty *)
(* Reverse a list. *)
let rec rev l =
    match l with
    | [] -> []
    | hd :: tl -> rev tl @ [hd]
;;

(* Palindrome *)
(* Beginner difficulty *)
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
    | (One  hd) :: tl -> hd :: flatten tl
    | (Many hd) :: tl -> flatten hd @ flatten tl
;;

(* Eliminate Duplicates *)
(* Intermediate difficulty *)
(* Eliminate consecutive duplicates of list elements. *)
let rec compress l =
    match l with
    | [] -> []
    | hd :: tl ->
            let tl = compress tl in
            if List.mem hd tl then tl else hd :: tl
;;

(* Pack Consecutive Duplicates *)
(* Intermediate difficulty *)
(* Pack consecutive duplicates of list elements into sublists. *)
let rec pack l =
    match l with
    | [] -> [[]]
    | hd :: tl ->
            match pack tl with
            | [] :: tl -> [hd] :: tl
            | (phd :: ptl as pl) :: tl when hd = phd -> (hd :: pl) :: tl
            | _ as pl -> [hd] :: pl
;;

(* Run-Length Encoding *)
let rec encode l =
    match l with
    | [] -> []
    | hd :: tl ->
            match encode tl with
            | (n, en_el) :: en_tl when en_el = hd -> (n+1, en_el) :: en_tl
            | _ as en_l -> (1, hd) :: en_l
;;

(* Decode a Run-Length Encoded List *)
(* Intermediate difficulty *)
(* Given a run-length code list generated as specified in the previous problem, construct its uncompressed version. *)
let rec decode l =
    match l with
    | [] -> []
    | (1, el) :: tl -> el :: decode tl
    | (n, el) :: tl -> el :: decode ((n-1, el) :: tl)
;;

(* Duplicate the Elements of a List *)
(* Beginner difficulty *)
(* Duplicate the elements of a list. *)
let rec duplicate l =
    match l with
    | [] -> []
    | hd :: tl -> hd :: hd :: duplicate tl
;;

(* Replicate the Elements of a List a Given Number of Times *)
(* Intermediate difficulty *)
(* Replicate the elements of a list a given number of times. *)
let rec replicate l n =
    let rec repeat el n =
        if n = 0 then []
        else el :: repeat el (n-1)
in
    match l with
    | [] -> []
    | hd :: tl -> (repeat hd n) @ (replicate tl n)
;;

(* Drop Every N'th Element From a List *)
(* Intermediate difficulty *)
(* Drop every N'th element from a list. *)
let drop l n =
    let rec _drop l n i =
        match (l, i) with
        | ([], _) -> []
        | (hd :: tl, 1) -> _drop tl n n
        | (hd :: tl, _)-> hd :: _drop tl n (i-1)
in
    if n <= 0
    then raise (Failure "drop: n must not be <1")
    else _drop l n n
;;

(* Split a List Into Two Parts; The Length of the First Part Is Given *)
(* Beginner difficulty *)
(* Split a list into two parts; the length of the first part is given. *)
(* If the length of the first part is longer than the entire list, then the first part is the list and the second part is empty. *)
let rec split l n =
    match (l, n) with
    | ([], _) -> ([], [])
    | (hd :: tl, 0) -> ([], l)
    | (hd :: tl, _) ->
            match split tl (n-1) with
            | ([], stl)  -> ([hd], stl)
            | (shd, stl) -> (hd :: shd, stl)
;;

(* Extract a Slice From a List *)
(* Intermediate difficulty *)
(* Given two indices, i and k, the slice is the list containing the elements 
   between the i'th and k'th element of the original list (both limits included). *)
(* Start counting the elements with 0 (this is the way the List module numbers elements). *)
let rec slice l a b =
    match (l, a, b) with
    | (hd :: tl, 0, 0) -> [hd]
    | (hd :: tl, 0, b) -> hd :: slice tl 0 (b-1)
    | (hd :: tl, a, b) -> slice tl (a-1) (b-1)
    | ([], _, _) -> raise (Failure "slice: index out of bounds")

