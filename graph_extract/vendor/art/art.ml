type tree

external create: unit -> tree = "call_art_create"
external put: tree -> string -> int -> unit = "call_art_put"
external get: tree -> string -> int = "call_art_get"
external length: tree -> int = "call_art_length"
external remove: tree -> string -> bool = "call_art_remove"
external iter: tree -> (string -> int -> unit) -> unit = "call_art_iter"
external iter_prefix: tree -> string -> (string -> int -> unit) -> unit = "call_art_iter_prefix"
external sum : tree -> int = "call_art_sum"

let update t k f = 
  put t k (f (get t k))

let fold t folder init = 
  let res = ref init in 
  iter t (fun k v -> res := (folder k v !res));
  !res

let fold_prefix t k folder init = 
  let res = ref init in 
  iter_prefix t k (fun k v -> res := (folder k v !res));
  !res

let sum_prefix t k = fold_prefix t k (fun _k v prev -> v + prev) 0

let merge a b mergefn = 
  let res = create () in 
  iter a (fun k v -> 
    let resv = try (
      let v2 = get b k in 
      mergefn v v2
    ) with Not_found -> v in 
    put res k resv;
  );
  res

let items tree = 
  let res = ref [] in 
  iter tree (fun k c -> 
    res := (k, c) :: !res;
  );
  !res

let incr tree k inc = 
  try
    put tree k ((get tree k) + inc)
  with Not_found -> put tree k 1