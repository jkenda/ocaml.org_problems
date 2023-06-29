let timeit f_name f arg =
    let open Format in
    let t0 = Sys.time () in
    let _ = f arg in
    let t1 = Sys.time () in
    printf "%.3f ms <- %s\n" ((t1 -. t0) *. 1000.) f_name

