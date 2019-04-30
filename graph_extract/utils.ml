
let gzip_files_stream basepath glob =
  Unix.open_process_in (Printf.sprintf "find %s -name %s | xargs -n 1 gunzip -c" basepath glob)

let xz_files_stream basepath glob =
  Unix.open_process_in (Printf.sprintf "find %s -name %s | xargs -n 1 xzcat" basepath glob)

let line_reader instream =
  (fun () -> try Some(input_line instream) with End_of_file -> None)

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

let with_out infname f = 
  let fd = open_out infname in 
  try_finalize (fun () -> f fd) (fun () -> close_out fd)
