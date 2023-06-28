(* Tail of a List *)
(* Beginner difficulty *)
(* Write a function last : 'a list -> 'a option that returns the last element of a list *)
let rec last = function
    | [] -> None
    | [el] -> Some el
    | hd :: tl -> last tl
;;
assert (last ["a" ; "b" ; "c" ; "d"] = Some "d");;
assert (last [] = None);;

(* Last Two Elements of a List *)
(* Beginner difficulty *)
(* Find the last but one (last and penultimate) elements of a list. *)
let rec last_two = function
    | [] | [_] -> None
    | [a ; b] -> Some (a, b)
    | hd :: tl -> last_two tl
;;
assert (last_two ["a"; "b"; "c"; "d"] = Some ("c", "d"));;
assert (last_two ["a"] = None);;

(* N'th Element of a List *)
(* Beginner difficulty *)
(* Find the N'th element of a list. *)
let rec nth n = function
    | [] -> raise (Failure "nth")
    | hd :: tl -> if n = 0 then hd else nth (n - 1) tl
;;
assert (nth 2 ["a"; "b"; "c"; "d"; "e"] = "c");;

(* Length of a List *)
(* Beginner difficulty *)
(* Find the number of elements of a list. *)
let len list =
    let rec aux acc = function
        | [] -> acc
        | hd :: tl -> aux (acc + 1) tl
    in
    aux 0 list
;;
assert (len ["a"; "b"; "c"] = 3);;
assert (len [] = 0);;

(* Reverse a List *)
(* Beginner difficulty *)
(* Reverse a list. *)
let rev list =
    let rec aux acc = function
        | [] -> acc
        | hd :: tl -> aux (hd :: acc) tl
    in
    aux [] list
;;
assert (rev ["a"; "b"; "c"] = ["c"; "b"; "a"]);;

(* Palindrome *)
(* Beginner difficulty *)
(* Find out whether a list is a palindrome. *)
let is_palindrome list =
    list = rev list
;;
assert (is_palindrome ["x"; "a"; "m"; "a"; "x"]);;
assert (not (is_palindrome ["a"; "b"]));;

(* Flatten a List *)
(* Intermediate difficulty *)
(* Flatten a nested list structure. *)
type 'a node =
    | One of 'a 
    | Many of 'a node list
;;
let rec flatten = function
    | [] -> []
    | (One  hd) :: tl -> hd :: flatten tl
    | (Many hd) :: tl -> flatten hd @ flatten tl
;;
assert (flatten [One "a"; Many [One "b"; Many [One "c" ;One "d"]; One "e"]] = ["a"; "b"; "c"; "d"; "e"]);;

(* Eliminate Duplicates *)
(* Intermediate difficulty *)
(* Eliminate consecutive duplicates of list elements. *)
let rec compress = function
    | a :: (b :: _ as tl) -> if a = b then compress tl else a :: compress tl
    | other -> other
;;
assert (compress ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"] = ["a"; "b"; "c"; "a"; "d"; "e"]);;

(* Pack Consecutive Duplicates *)
(* Intermediate difficulty *)
(* Pack consecutive duplicates of list elements into sublists. *)
let rec pack = function
    | [] -> []
    | hd :: tl ->
            match pack tl with
            | [] :: tl -> [hd] :: tl
            | (phd :: ptl as pl) :: tl when hd = phd -> (hd :: pl) :: tl
            | _ as pl -> [hd] :: pl
;;
assert (pack ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "d"; "e"; "e"; "e"; "e"]
= [["a"; "a"; "a"; "a"]; ["b"]; ["c"; "c"]; ["a"; "a"]; ["d"; "d"]; ["e"; "e"; "e"; "e"]]);;

(* Run-Length Encoding *)
let encode list =
    let rec aux acc = function
        | [] -> acc
        | hd :: tl ->
                match acc with
                | (n, en_el) :: en_tl when en_el = hd -> aux ((n + 1, en_el) :: en_tl) tl
                | _ as en_l -> aux ((1, hd) :: en_l) tl
    in
    aux [] list
        |> List.rev
;;
assert (encode ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"]
= [(4, "a"); (1, "b"); (2, "c"); (2, "a"); (1, "d"); (4, "e")]);;

(* Decode a Run-Length Encoded List *)
(* Intermediate difficulty *)
(* Given a run-length code list generated as specified in the previous problem, construct its uncompressed version. *)
let decode list =
    let rec aux acc = function
        | [] -> acc
        | (1, el) :: tl -> aux (el :: acc) tl
        | (n, el) :: tl -> aux (el :: acc) ((n - 1, el) :: tl)
    in
    aux [] list
        |> List.rev
;;
assert (decode [(4, "a"); (1, "b"); (2, "c"); (2, "a"); (1, "d"); (4, "e")]
= ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"]);;

(* Duplicate the Elements of a List *)
(* Beginner difficulty *)
(* Duplicate the elements of a list. *)
let duplicate list =
    let rec aux acc = function
        | [] -> acc
        | hd :: tl -> aux (hd :: hd :: acc) tl
    in
    aux [] list
        |> List.rev
;;
assert (duplicate ["a"; "b"; "c"; "c"; "d"] = ["a"; "a"; "b"; "b"; "c"; "c"; "c"; "c"; "d"; "d"]);;

(* Replicate the Elements of a List a Given Number of Times *)
(* Intermediate difficulty *)
(* Replicate the elements of a list a given number of times. *)
let rec replicate list n =
    let rec aux acc m = function
        | [] -> acc
        | hd :: tl when m = 0 -> aux acc n tl
        | hd :: tl as l -> aux (hd :: acc) (m - 1) l
in
    aux [] n list
        |> List.rev
;;
assert (replicate ["a"; "b"; "c"] 3 = ["a"; "a"; "a"; "b"; "b"; "b"; "c"; "c"; "c"]);;

(* Drop Every N'th Element From a List *)
(* Intermediate difficulty *)
(* Drop every N'th element from a list. *)
let drop list n =
    let rec aux acc i = function
        | [] -> acc
        | hd :: tl when i = 1 -> aux acc n tl 
        | hd :: tl -> aux (hd :: acc) (i - 1) tl
in
    aux [] n list
        |> List.rev
;;
assert (drop ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j"] 3 = ["a"; "b"; "d"; "e"; "g"; "h"; "j"]);;

(* Split a List Into Two Parts; The Length of the First Part Is Given *)
(* Beginner difficulty *)
(* Split a list into two parts; the length of the first part is given. *)
(* If the length of the first part is longer than the entire list, then the first part is the list and the second part is empty. *)
let split list n =
    let rec aux acc1 acc2 list n =
        match (list, n) with
        | ([], _) -> (acc1, acc2)
        | (hd :: tl, 0) -> aux acc1 (hd :: acc2) tl 0
        | (hd :: tl, _) -> aux (hd :: acc1) acc2 tl (n - 1)
    in
    let (a, b) = aux [] [] list n in
    (List.rev a, List.rev b)
;;
assert (split ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j"] 3 = (["a"; "b"; "c"], ["d"; "e"; "f"; "g"; "h"; "i"; "j"]));;
assert (split ["a"; "b"; "c"; "d"] 5 = (["a"; "b"; "c"; "d"], []));;

(* Extract a Slice From a List *)
(* Intermediate difficulty *)
(* Given two indices, i and k, the slice is the list containing the elements 
   between the i'th and k'th element of the original list (both limits included). *)
(* Start counting the elements with 0 (this is the way the List module numbers elements). *)
let slice list a b =
    let rec aux acc list a b =
        match (list, a, b) with
        | (hd :: tl, 0, 0) -> hd :: acc
        | (hd :: tl, 0, b) -> aux (hd :: acc) tl 0 (b - 1)
        | (hd :: tl, a, b) -> aux acc tl (a - 1) (b - 1)
        | ([], _, _) -> raise (Failure "slice: index out of bounds")
    in
    List.rev (aux [] list a b)
;;
assert (slice ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j"] 2 6 = ["c"; "d"; "e"; "f"; "g"]);;

