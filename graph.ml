type 'a node =
    Node of 'a * 'a node list
;;
let get_el (Node (el, _)) = el;;

let find_dfs graph goal =
    let rec search_node (Node (self, children)) =
        let rec search_list = function
            | [] -> None
            | hd :: tl ->
                    match search_node hd with
                    | None -> search_list tl
                    | r -> r
        in
        if self = goal then Some [self]
        else
            match search_list children with
            | Some r -> Some (self :: r)
            | None -> None
    in
    search_node graph
;;
assert (find_dfs (Node ('a', [Node ('b', []); Node ('c', [])])) 'h' = None);;
assert (find_dfs (Node ('a', [Node ('b', []); Node ('c', [Node ('h', [])])])) 'h' = Some ['a'; 'c'; 'h']);;

let find_bfs graph goal =
    let create_paths path =
        List.map (fun (Node (child, _)) -> child :: path)
    in
    let rec aux paths queue =
        match paths, queue with
        | [], [] -> None
        | path :: paths, Node (self, children) :: queue ->
                if self = goal then Some (List.rev path)
                else aux (paths @ create_paths path children) (queue @ children)
        | _, _ -> raise (Failure "unreachable")
    in
    aux [[get_el graph]] [graph]
;;
assert (find_bfs (Node ('a', [Node ('b', []); Node ('c', [])])) 'c' = Some ['a'; 'c']);;
assert (find_bfs (Node ('a', [Node ('b', []); Node ('c', [Node ('h', [])])])) 'h' = Some ['a'; 'c'; 'h']);;

