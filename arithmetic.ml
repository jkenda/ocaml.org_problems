#use "lists.ml";;

(* Determine Whether a Given Integer Number Is Prime *)
(* Intermediate difficulty *)
(* Determine whether a given integer number is prime. *)
let is_prime n =
    let rec is_not_divisor d =
        d * d > n || (n mod d != 0 && is_not_divisor (d + 1)) in
    n != 1 && is_not_divisor 2
;;
assert (not (is_prime 1));;
assert (is_prime 7);;
assert (not (is_prime 12));;

(* Determine the Greatest Common Divisor of Two Positive Integer Numbers *)
(* Intermediate difficulty *)
(* Determine the greatest common divisor of two positive integer numbers. *)
(* Use Euclid's algorithm. *)
let rec gcd a b =
    if a = 0 then b
    else gcd (b mod a) a
;;
assert (gcd 13 27 = 1);;
assert (gcd 20536 7826 = 2);;

(* Determine Whether Two Positive Integer Numbers Are Coprime *)
(* Beginner difficulty *)
(* Determine whether two positive integer numbers are coprime. *)
(* Two numbers are coprime if their greatest common divisor equals 1. *)
let coprime a b =
    gcd a b = 1
;;
assert (coprime 13 27);;
assert (not (coprime 20536 7826));;

(* Calculate Euler's Totient Function Φ(m) *)
(* Intermediate difficulty *)
(* Euler's so-called totient function φ(m) is defined as the number of positive integers r (1 ≤ r < m) that are coprime to m. We let φ(1) = 1. *)
(* Find out what the value of φ(m) is if m is a prime number.
   Euler's totient function plays an important role in one of the most widely used public key cryptography methods (RSA).
   In this exercise you should use the most primitive method to calculate this function (there are smarter ways that we shall discuss later). *)
let phi x =
    let rec aux acc = function
        | 0 -> acc
        | y -> aux (if coprime x y then acc + 1 else acc) (y - 1)
    in
    aux 0 (x - 1)
;;
assert (phi 10 = 4);;

(* Determine the Prime Factors of a Given Positive Integer *)
(* Intermediate difficulty *)
(* Construct a flat list containing the prime factors in ascending order. *)
let factors n =
    let rec next_prime n =
        if is_prime n then n
        else next_prime (n + 1)
    in
    let rec aux n = function
        | _ when n <= 1 -> []
        | p when n mod p = 0 -> p :: aux (n / p) p
        | p -> aux n (next_prime (p + 1))
    in
    aux n 2
;;
assert (factors 315 = [3; 3; 5; 7]);;

(* Calculate Euler's Totient Function Φ(m) (Improved) *)
(* Intermediate difficulty *)
(* See problem "Calculate Euler's totient function φ(m)" for the definition of Euler's totient function.
   If the list of the prime factors of a number m is known in the form of the previous problem
   then the function phi(m) can be efficiently calculated as follows:
   Let [(p1, m1); (p2, m2); (p3, m3); ...] be the list of prime factors (and their multiplicities) of a given number m.
   Then φ(m) can be calculated with the following formula: *)
(* φ(m) = (p1 - 1) × p1m1 - 1 × (p2 - 1) × p2m2 - 1 × (p3 - 1) × p3m3 - 1 × ⋯ *)
let rec phi_improved x =
    let pow x n =
        let rec aux acc = function
            | 0 -> acc
            | n -> aux (acc * x) (n - 1)
        in
        aux 1 n
    in
    let rec aux acc = function
        | [] -> acc
        | (m, p) :: tl -> aux (acc * (p - 1) * pow p (m - 1)) tl
    in
    factors x
        |> encode
        |> aux 1
;;
assert (phi_improved 10 = 4);;
assert (phi_improved 13 = 12);;

let timeit f f_name arg =
    let open Format in
    let t0 = Sys.time () in
    let result = f arg in
    let t1 = Sys.time () in
    printf "%d; %f ms <- %s\n" result ((t1 -. t0) *. 1000.) f_name;;
;;
let integer = 1234567;;
timeit phi "phi" integer;;
timeit phi_improved "phi_improved" integer;;

(* A List of Prime Numbers *)
(* Beginner difficulty *)
(* Given a range of integers by its lower and upper limit, construct a list of all prime numbers in that range. *)
let all_primes lower upper =
    let rec aux acc = function
        | n when n > upper -> acc
        | n -> if is_prime n then aux (n :: acc) (n + 1) else aux acc (n + 1)
    in
    aux [] lower
;;
assert (List.length (all_primes 2 7920) = 1000);;

(* Goldbach's Conjecture *)
(* Intermediate difficulty *)
(* Goldbach's conjecture says that every positive even number greater than 2 is the sum of two prime numbers.
   Example: 28 = 5 + 23.
   It is one of the most famous facts in number theory that has not been proved to be correct in the general case.
   It has been numerically confirmed up to very large numbers.
   Write a function to find the two prime numbers that sum up to a given even integer. *)
let goldbach n =
    let rec next_prime n =
        if is_prime n then n
        else next_prime (n + 1)
    in
    let rec aux i j =
        match (i, j) with
        | _ when i >= n -> raise (Failure "Goldbach's conjecture proven wrong.")
        | _ when j >= n -> aux (next_prime (i + 1)) 1
        | _ when i + j = n -> (i, j)
        | _ -> aux i (next_prime (j + 1))
    in
    if n <= 2 || n mod 2 != 0 then raise (Failure "n must be an even number greater than 2")
    else aux 2 (n / 2 - 2)
;;
assert (goldbach 28 = (5, 23));;

(* A List of Goldbach Compositions *)
(* Intermediate difficulty *)
(* Given a range of integers by its lower and upper limit, print a list of all even numbers and their Goldbach composition. *)
(* In most cases, if an even number is written as the sum of two prime numbers, one of them is very small.
   Very rarely, the primes are both bigger than say 50.
   Try to find out how many such cases there are in the range 2..3000. *)
let goldbach_list lower upper =
    let rec next_even n = if n mod 2 = 0 then n else n + 1 in
    let rec aux acc n =
        if n > upper then acc
        else aux ((n, goldbach n) :: acc) (n + 2)
    in
    aux [] (next_even lower)
        |> List.rev
;;
assert (goldbach_list 9 20
= [(10, (3, 7)); (12, (5, 7)); (14, (3, 11)); (16, (3, 13)); (18, (5, 13)); (20, (3, 17))]);;

