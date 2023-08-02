type distance = int;;
type 'a node =
    Node of 'a * ('a node * distance) list
;;
let get_el (Node (el, _)) = el;;

exception Unreachable of string;;

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
        else
            search_list children
    in
    search_node [] 0 graph
        |> Option.map (fun (path, distance) -> (List.rev path, distance))
;;
assert (find_dfs (Node ('a', [Node ('b', []), 1; Node ('c', []), 2])) ['h'] = None);;
assert (find_dfs (Node ('a', [Node ('b', []), 1; Node ('c', []), 2])) ['c'] = Some (['a'; 'c'], 2));;
assert (find_dfs (Node ('a', [Node ('b', []), 1; Node ('c', [Node ('h', []), 3]), 2])) ['h'] = Some (['a'; 'c'; 'h'], 5));;

let find_bfs graph goals =
    let create_paths_dists path sum children =
        children
        |> List.map (fun (Node (child, _), dist) -> child :: path, sum + dist)
        |> List.split
    in
    let rec aux paths sums queue =
        match paths, sums, queue with
        (* queue is exhausted, none of the goals found *)
        | [], [], [] -> None
        (* there are still elements in the queue *)
        | path :: paths, dist :: dists, (Node (self, children), _) :: queue ->
                (* goal found *)
                if List.mem self goals then Some (List.rev path, dist)
                else
                    let (new_paths, new_dists) = create_paths_dists path dist children in
                    aux (paths @ new_paths) (dists @ new_dists) (queue @ children)
        | _ -> raise (Unreachable "the 3 lists are always of the same length")
    in
    aux [[get_el graph]] [0] [graph, 0]
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


let rec s = Node ('s', [a, 3; b, 2; c, 2])
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

assert (find_bfs s ['l'; 'o'] = Some (['s'; 'a'; 'e'; 'l'], 9));;
assert (find_dfs s ['l'; 'o'] = Some (['s'; 'a'; 'd'; 'j'; 'k'; 'e'; 'l'], 10));;
assert (find_ids s ['l'; 'o'] = find_bfs s ['l'; 'o'])

