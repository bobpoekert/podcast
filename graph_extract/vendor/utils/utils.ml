open Bigarray

let rec _read_lines res inf = 
  try _read_lines ((input_line inf) :: res) inf with End_of_file -> res

let read_lines inf = _read_lines [] inf

let reducer_catchall f = 
  (fun a b -> try (f a b) with _ -> a)

let reducer_catchall_r f = 
  (fun a b -> try (f a b) with _ -> b)

let rec _remove_none res l =
  match l with 
  | [] -> [] 
  | None :: t -> _remove_none res t 
  | Some(v) :: t -> _remove_none (v :: res) t

let remove_none l = _remove_none [] l

let rec line_seq instream = 
  (fun () -> 
    try
      Seq.Cons(input_line instream, line_seq instream)
    with End_of_file -> Seq.Nil
  )

let rec iter_lines f ins =
  try (
    f (input_line ins);
    iter_lines f ins
  ) with End_of_file -> ()

let rec fold_lines f acc ins = 
  try (
    let res = f acc (input_line ins) in 
    fold_lines f res ins
  ) with End_of_file -> acc

let find_glob basepath glob =
  read_lines (Unix.open_process_in (Printf.sprintf "find %s -name %s" basepath glob))

let gzip_files_stream basepath glob =
  Unix.open_process_in (Printf.sprintf "find %s -name %s | xargs -n 1 gunzip -c" basepath glob)

let xz_files_stream basepath glob =
  Unix.open_process_in (Printf.sprintf "find %s -name %s | xargs -n 1 xzcat" basepath glob)

let gunzip fname = 
  Unix.open_process_in (Printf.sprintf "gunzip -c %s" fname)

let xunzip fname =
  Unix.open_process_in (Printf.sprintf "xzcat %s" fname)

let table_increment t k =
  try
    (Hashtbl.replace t k ((Hashtbl.find t k) + 1); ())
  with Not_found ->
    (Hashtbl.replace t k 1; ())

let iter_pairwise thunk lst =
  List.iter (fun a -> 
    List.iter (fun b -> thunk a b) lst
  ) lst

let assert_some v =
  match v with 
  | Some(v) -> v
  | None -> raise (Invalid_argument "unexpected None")

let find_opt h k =
  try Some(Hashtbl.find h k) with Not_found -> None

let table_into_table mergefn drain src =
  Hashtbl.iter (fun k v ->
    match find_opt drain k with 
    | None -> Hashtbl.add drain k v 
    | Some(v2) -> Hashtbl.replace drain k (mergefn v2 v)) src;
  drain

let table_values h = 
  let hlen = Hashtbl.length h in 
  match Hashtbl.fold (fun _k v acc -> 
    let off, acc = match acc with | Some(v) -> v | None -> (0, Array.make hlen v) in
    Array.set acc off v;
    Some(off + 1, acc)
  ) h None with 
  | None -> [| |]
  | Some(_, v) -> v

let table_keys h = 
  let hlen = Hashtbl.length h in 
  match Hashtbl.fold (fun k _v acc -> 
    let off, acc = match acc with | Some(k) -> k | None -> (0, Array.make hlen k) in
    Array.set acc off k;
    Some(off + 1, acc)
  ) h None with 
  | None -> [| |]
  | Some(_, k) -> k


let clean_url url = 
  let colon_idx = String.index url ':' in 
  String.sub url colon_idx ((String.length url) - colon_idx)

let try_finalize f finally =
  let res = try f () with exn -> finally (); raise exn in
  finally ();
  res

let with_exit f = 
  try Printexc.print f (); exit 0 with _exn -> Printexc.print_backtrace stderr; exit 1

let with_out infname f = 
  let fd = open_out infname in 
  try_finalize (fun () -> f fd) (fun () -> close_out fd)

let load_genarray fname dtype = 
  let size_bytes = (Unix.stat fname).st_size in 
  let item_size = Bigarray.kind_size_in_bytes dtype in 
  let n_items = size_bytes / item_size in 
  let fd = Unix.openfile fname [] 0o640 in 
  let arr = Unix.map_file fd dtype c_layout false [| n_items |] in
  arr

let load_array fname dtype = 
  array1_of_genarray (load_genarray fname dtype)

let load_array2 fname dtype width = 
  let g = load_genarray fname dtype in 
  let size = Array.get (Genarray.dims g) 0 in 
  let height = size / width in 
  reshape g [| width; height |] |> array2_of_genarray

let array2_with_file fname dtype dim1 dim2 thunk =
  let target_fd = Unix.openfile fname [Unix.O_RDWR; Unix.O_CREAT; Unix.O_APPEND] 0o640 in
  let size_bytes = dim1 * dim2 * (kind_size_in_bytes dtype) in 
  let _ = Unix.ftruncate target_fd size_bytes in 
  let target = Unix.map_file target_fd dtype C_layout true [| dim1; dim2 |] in 
  let res = Bigarray.array2_of_genarray target in
  let v = thunk res in 
  Unix.close target_fd;
  v

let rec _array_filteri thunk arr res res_off arr_off arr_len = 
  if arr_off >= arr_len then (Array.sub res 0 res_off) else (
    let v = Array.get arr arr_off in 
    if (thunk arr_off v) then (
      Array.set res res_off v;
      _array_filteri thunk arr res (res_off + 1) (arr_off + 1) arr_len
    ) else _array_filteri thunk arr res res_off (arr_off + 1) arr_len
  )

let array_filteri thunk arr = 
  let len = Array.length arr in 
  if len < 1 then arr else (
    let res = Array.make len (Array.get arr 0) in 
    _array_filteri thunk arr res 0 0 len
  )

let load_marshal fname = 
  let fd = open_in fname in
  try_finalize (fun () -> Marshal.from_channel fd) (fun () -> 
    close_in fd
  )

let rec _binary_search_idx arr k l r =
  if l <= r then 
    let m = ((l + r) / 2) in 
    let v = Array1.get arr m in 
    if v < k then 
      _binary_search_idx arr k (m + 1) r
    else if v > k then 
      _binary_search_idx arr k l (m - 1)
    else (`Found m) 
  else (`Missing r)

let binary_search_idx arr k = _binary_search_idx arr k 0 ((Array1.dim arr) - 1)

let binary_search arr k = 
  match binary_search_idx arr k with 
  | `Found(m) -> m 
  | `Missing(_) -> raise Not_found

let rec _binary_search_idx_v arr k l r reverse =
  if l <= r then 
    let m = ((l + r) / 2) in 
    let v = Array.get arr m in 
    if (if reverse then k < v else v < k) then 
      _binary_search_idx_v arr k (m + 1) r reverse
    else if (if reverse then k > v else v > k) then 
      _binary_search_idx_v arr k l (m - 1) reverse
    else m 
  else r

let binary_search_idx_v ?(reverse=false) arr k = _binary_search_idx_v arr k 0 ((Array.length arr) - 1) reverse

let binary_search_v ?(reverse=false) arr k = 
  let idx = binary_search_idx_v arr k ~reverse:reverse in
  if idx >= 0 && (Array.get arr idx) == k then idx else raise Not_found

let url_hash url = 
  let url = List.nth (String.split_on_char ':' url) 1 in 
  let url = Printf.sprintf ":%s" url in 
  Murmur.murmur_hash url

let rec _partition parts inp = 
  match inp with 
  | [] -> parts
  | _item :: inp -> (
    let part = List.hd parts in 
    let parts = List.tl parts in 
    _partition (List.append parts [part]) inp
  )

let partition n_parts inp = 
  let tot_len  = Array.length inp in 
  let part_size = tot_len / n_parts in 
  let res = Array.make n_parts inp in 
  for i = 0 to (n_parts - 1) do 
    Array.set res i (Array.sub inp (part_size * i) part_size)
  done;
  res

let spawn_worker combiner part pipe_out = 
  if Unix.fork () == 0 then (
    let res = combiner part in 
    let _ = print_endline "---" in 
    let chan_out = Unix.out_channel_of_descr pipe_out in 
    Marshal.to_channel chan_out res [];
    flush_all ();
    exit 0;
  )

let maprange f n =
  let first = f 0 in 
  let res = Array.make n first in 
  Array.set res 0 first;
  for i = 1 to (n - 1) do 
    Array.set res i (f i)
  done;
  res

let contains l v = List.exists (fun vv -> vv == v) l
let remove a b = List.filter (fun v -> not (contains b v)) a

let rec reduce_fds fds reducer res = 
  match fds with
  | [] -> res 
  | h :: t -> (
      let chan = Unix.in_channel_of_descr h in 
      let v = Marshal.from_channel chan in 
      reduce_fds t reducer (reducer res v)
  )

let rec consume_pipes pipes reducer res = 
  match pipes with 
  | [] -> res 
  | _ -> (
    let read, _wirte, _ex = Unix.select pipes [] [] (-1.0) in
    let res = reduce_fds read reducer res in 
    List.iter Unix.close read;
    consume_pipes (remove pipes read) reducer res
  )

let corecount () = Corecount.count () |> Nativeint.to_int

let parmap inp combiner reducer reducer_init = 
  let ncores = corecount () in 
  let parts = partition ncores inp in 
  let pipes = maprange (fun _n -> Unix.pipe ~cloexec:false ()) ncores in
  let _ = Array.iter2 (fun part (_pread, pwrite) -> spawn_worker combiner part pwrite) parts pipes in 
  consume_pipes (Array.to_list (Array.map (fun (pread, _pwrite) -> pread) pipes)) reducer reducer_init

let parrun thunk = 
  let ncores = corecount () in 
  let pids = maprange (fun i -> 
    let pid = Unix.fork () in 
    if pid == 0 then 
      (with_exit (fun () -> thunk i; ); 1)
    else pid
  ) ncores in
  Array.iter (fun pid -> let _ = Unix.waitpid [] pid in ()) pids

let argsort_generic comparator arr = 
  let n_items = Array.length arr in 
  let res = Array.make n_items 0 in 
  for i = 0 to (n_items - 1) do 
    Array.set res i i
  done;
  Array.fast_sort (fun l r -> 
    let lv = Array.get arr l in 
    let rv = Array.get arr r in 
    (comparator lv rv)
  ) res;
  res

let argsort arr = argsort_generic (-) arr

let get_indexes arr idxes = 
  Array.map (Array.get arr) idxes

let get_indexes_inplace arr idxes = 
  let res = get_indexes arr idxes in 
  Array.blit res 0 arr 0 (Array.length arr);