type distance = int;;
type 'a node =
    Node of 'a * ('a node * distance) list
;;
let get_el (Node (el, _)) = el;;

exception Unreachable of string;;

(* depth first search *)
let find_dfs graph goals =
    let rec search_node path sum (Node (self, children)) =
        let rec search_list = function
            | [] -> None
            | (child, dist) :: tl ->
                    match search_node (self :: path) (sum + dist) child with
                    | None -> search_list tl
                    | r -> r
        in
        (* goal reached *)
        if List.mem self goals then Some (self :: path, sum)
        (* prevent cycling *)
        else if List.mem self path then None
        (* search children *)
        else search_list children
    in
    search_node [] 0 graph
        |> Option.map (fun (path, distance) -> (List.rev path, distance))
;;
assert (find_dfs (Node ('a', [Node ('b', []), 1; Node ('c', []), 2])) ['h'] = None);;
assert (find_dfs (Node ('a', [Node ('b', []), 1; Node ('c', []), 2])) ['c'] = Some (['a'; 'c'], 2));;
assert (find_dfs (Node ('a', [Node ('b', []), 1; Node ('c', [Node ('h', []), 3]), 2])) ['h'] = Some (['a'; 'c'; 'h'], 5));;

(* breadth first search *)
let find_bfs graph goals =
    let add_to_queue path sum children queue =
        queue @ List.map (fun ((Node (tag, _), dist) as child) -> tag :: path, sum + dist, child) children
    in
    let rec aux = function
        (* queue is exhausted, none of the goals found *)
        | [] -> None
        (* there are still elements in the queue *)
        | (path, dist, (Node (self, children), _)) :: queue ->
                (* goal found *)
                if List.mem self goals then Some (List.rev path, dist)
                else aux (add_to_queue path dist children queue)
    in
    aux [([get_el graph], 0, (graph, 0))]
;;
assert (find_bfs (Node ('a', [Node ('b', []), 1; Node ('c', []), 2])) ['c'] = Some (['a'; 'c'], 2));;
assert (find_bfs (Node ('a', [Node ('b', []), 1; Node ('c', [Node ('h', []), 3]), 2])) ['h'] = Some (['a'; 'c'; 'h'], 5));;

type 'a container =
    | Bottom
    | Limit
    | Found of 'a
;;
let container_map f = function
    | Bottom | Limit as bl -> bl
    | Found x -> Found (f x)
;;

(* iterative deepeining depth first search *)
let find_ids graph goals =
    let find_dfs_limited limit =
        let rec search_node path sum (Node (self, children)) limit =
            let rec search_list = function
                | [] -> Bottom
                | (child, dist) :: tl ->
                        match search_node (self :: path) (sum + dist) child (limit - 1) with
                        | Limit ->
                                (match search_list tl with
                                 | Bottom | Limit -> Limit
                                 | r -> r)
                        | Bottom -> search_list tl
                        | r -> r
            in
            (* goal reached *)
            if List.mem self goals then Found (self :: path, sum)
            (* limit reached *)
            else if limit < 0 then Limit
            (* prevent cycling *)
            else if List.mem self path then Bottom
            (* search children *)
            else
                search_list children
        in
        search_node [] 0 graph limit
            |> container_map (fun (path, distance) -> (List.rev path, distance))
    in
    let rec aux limit =
        match find_dfs_limited limit with
        | Bottom -> None
        | Limit -> aux (limit + 1)
        | Found r -> Some r
    in
    aux 0
;;
assert (find_ids (Node ('a', [Node ('b', []), 1; Node ('c', []), 2])) ['c'] = Some (['a'; 'c'], 2));;
assert (find_ids (Node ('a', [Node ('b', []), 1; Node ('c', [Node ('h', []), 3]), 2])) ['h'] = Some (['a'; 'c'; 'h'], 5));;


let rec graph = Node ('s', [a, 3; b, 2; c, 2])
and a = Node ('a', [d, 3; e, 5])
and b = Node ('b', [e, 4; f, 3; g, 3])
and c = Node ('c', [g, 1; h, 5])
and d = Node ('d', [i, 3; j, 1])
and e = Node ('e', [j, 2; l, 1])
and f = Node ('f', [m, 3; n, 1])
and g = Node ('g', [])
and h = Node ('h', [o, 2; p, 1])
and i = Node ('i', [])
and j = Node ('j', [k, 1])
and k = Node ('k', [e, 1])
and l = Node ('l', [])
and m = Node ('m', [])
and n = Node ('n', [o, 2])
and o = Node ('o', [])
and p = Node ('p', []);;

let goals = ['l'; 'o'];;

let correct_h = function 's' -> 4 | 'a' -> 5 | 'b' -> 5 | 'c' -> 5 | 'd' -> 1 | 'e' -> 1 | 'f' -> 3 | 'g' -> 6 | 'h' -> 2
                       | 'i' -> 1 | 'j' -> 2 | 'k' -> 1 | 'l' -> 0 | 'm' -> 6 | 'n' -> 2 | 'o' -> 0 | 'p' -> 1
                       | _ -> raise (Failure "invalid node reached");;
let perfect_h = function 's' -> 7 | 'a' -> 6 | 'b' -> 5 | 'c' -> 7 | 'd' -> 4 | 'e' -> 1 | 'f' -> 3 | 'g' -> Int.max_int
                       | 'h' -> 2 | 'i' -> Int.max_int  | 'j' -> 3 | 'k' -> 2 | 'l' -> 0 | 'm' -> Int.max_int
                       | 'n' -> 2 | 'o' -> 0 | 'p' -> Int.max_int
                       | _ -> raise (Failure "invalid node reached");;

let incorrect_h = function 's' -> 4 | 'a' -> 5 | 'b' -> 6 | 'c' -> 5 | 'd' -> 1 | 'e' -> 4 | 'f' -> 3 | 'g' -> 6 | 'h' -> 5
                         | 'i' -> 1 | 'j' -> 2 | 'k' -> 1 | 'l' -> 2 | 'm' -> 6 | 'n' -> 2 | 'o' -> 0 | 'p' -> 1
                         | _ -> raise (Failure "invalid node reached");;

assert (find_bfs graph goals = Some (['s'; 'a'; 'e'; 'l'], 9));;
assert (find_dfs graph goals = Some (['s'; 'a'; 'd'; 'j'; 'k'; 'e'; 'l'], 10));;
assert (find_ids graph goals = find_bfs graph ['l'; 'o'])


(* greedy best-first search *)
let find_greedy graph goals heuristic =
    let rec search_node path sum (Node (self, children)) =
        let search_min_child children =
            let rec min ((min, _, _) as prev) (Node (tag, _) as hd, dist) =
                let h = heuristic tag in
                if h < min then (h, Some hd, dist) else prev
            in
            let (_, child, dist) = List.fold_left min (Int.max_int, None, 0) children in
            match child with
            | Some (Node (tag, _) as child) -> search_node (self :: path) (sum + dist) child
            | None -> None
        in
        (* goal reached *)
        if List.mem self goals then Some (self :: path, sum)
        (* prevent cycling *)
        else if List.mem self path then None
        (* search children *)
        else search_min_child children
    in
    search_node [] 0 graph
        |> Option.map (fun (path, distance) -> (List.rev path, distance))
;;
assert (find_greedy graph goals perfect_h = Some (['s'; 'b'; 'e'; 'l'], 7));;
assert (find_greedy graph ['l'; 'o'] correct_h = None);;

(* A* *)
let find_a' graph goals heur =
    let add_to_queue queue children path sum =
        let insert list ((Node (tag, _), dist) as node) =
            let (dist, _, _) as node = sum + dist, node, tag :: path in
            let rec insert' = function
                | [] -> [node]
                | (d, (Node (t, _), _), _) as hd :: tl as ls ->
                        if dist + heur tag <= d + heur t then node :: ls
                        else hd :: insert' tl
            in insert' list
        in
        List.fold_left insert queue children
    in
    let rec aux = function
        (* queue is exhausted, none of the goals found *)
        | [] -> None
        (* there are still elements in the queue *)
        | (dist, (Node (tag, children), _), path) :: queue ->
                (* goal found *)
                if List.mem tag goals then Some (List.rev path, dist)
                else aux (add_to_queue queue children path dist)
    in
    aux [(0, (graph, 0), [get_el graph])]
;;
assert (find_a' graph goals correct_h = Some (['s'; 'b'; 'e'; 'l'], 7));;
assert (find_a' graph goals perfect_h = find_a' graph goals correct_h);;

assert (find_a' graph goals incorrect_h = Some (['s'; 'b'; 'f'; 'n'; 'o'], 8));;

type ('a, 'b) result =
    | Result of 'a
    | Limit of 'b
;;

(* IDA* *)
let find_ida' graph goals heuristic =
    let find_a'limited limit =
        (* smallest of the distances that go over the limit *)
        let next_limit = ref None in
        let next_limit_is limit =
            match !next_limit with
            | None -> next_limit := Some limit
            | Some nl -> if limit < nl then next_limit := Some limit
        in
        (* add children's properties to queue *)
        let add_to_queue path sum children queue =
            let insert ((Node (tag, _), dist) as node) list =
                let (dist, _, _) as node = sum + dist, node, tag :: path in
                let dist_plus_h = dist + heuristic tag in
                let rec insert' = function
                    | [] -> [node]
                    | (d, (Node (t, _), _), _) :: _ as ls when dist_plus_h <= d + heuristic t -> node :: ls
                    | hd :: tl -> hd :: insert' tl
                in
                if dist_plus_h > limit then (next_limit_is dist_plus_h; list)
                else insert' list
            in
            List.fold_right insert children queue
        in
        let rec aux = function
            (* queue is exhausted, none of the goals found *)
            | [] -> Limit !next_limit
            (* there are still elements in the queue *)
            | (dist, (Node (tag, children), _), path) :: queue ->
                    (* goal found *)
                    if List.mem tag goals then Result (List.rev path, dist)
                    else aux (add_to_queue path dist children queue)
        in
        aux [(0, (graph, 0), [get_el graph])]
    in
    let rec find' limit =
        match find_a'limited limit with
        | Result path_dist -> Some path_dist
        | Limit None -> None
        | Limit (Some limit) -> find' limit
    in
    get_el graph
        |> heuristic
        |> find'
;;
assert (find_ida' graph goals incorrect_h = Some (['s'; 'b'; 'f'; 'n'; 'o'], 8));;
assert (find_ida' graph goals correct_h   = Some (['s'; 'b'; 'e'; 'l'], 7));;
assert (find_ida' graph goals perfect_h   = find_ida' graph goals correct_h);;

assert (find_ida' graph goals incorrect_h = find_a' graph goals incorrect_h);;
assert (find_ida' graph goals correct_h   = find_a' graph goals correct_h);;
assert (find_ida' graph goals perfect_h   = find_a' graph goals perfect_h);;


