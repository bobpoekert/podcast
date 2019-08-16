open Utils

(* tokenization *)

type text_value =
  | Text of string (* text blob, valid to tokenize *)
  | Tag of string (* opaque tag, should not be tokenized *)

let text_value_unwrap (v : text_value) =
  match v with
  | Text(v) -> v
  | Tag(v) -> v


let splitter_re = Re.Pcre.regexp "[^a-zA-Z0-9/]+"

let number_re = Re.Pcre.regexp "[0-9]"

let is_good_token t = 
  not (((String.length t) >= 15) ||
      (String.contains t '/') || 
      (Re.execp number_re t))

let rec _clean_tokens tokens res =
  match tokens with
  | [] -> res
  | h :: t ->
    match h with
    | `Text(v) -> _clean_tokens t ((String.lowercase_ascii v) :: res)
    | `Delim(_) -> _clean_tokens t res
let clean_tokens tokens = _clean_tokens tokens []



let tokenize_text (inp : text_value) = 
  match inp with
  | Tag(v) -> [v]
  | Text(v) -> (Re.split_full splitter_re v) |> clean_tokens |> List.filter is_good_token

let unwrap_text_value (inp : text_value) = 
  match inp with
  | Tag(v) -> v
  | Text(v) -> v

let bigrams unigrams = 
  try
    List.append unigrams (snd (List.fold_left (fun (prev, res) v -> (v, (Printf.sprintf "%s %s" v prev) :: res)) ((List.hd unigrams), []) (List.tl unigrams)))
  with Failure _ -> []
  
  (* histograms *)

let hist_update h k =
  let prev = Hashtbl.find h k in
  Hashtbl.replace h k (prev + 1)

let into_histogram h items =
  List.iter (hist_update h) items

let words_into_histogram h text =
  into_histogram h (tokenize_text text)  

type tree = (Art.tree * int)

let into_tree words : tree =
  let res = Art.create () in 
  List.iter (fun word -> Art.incr res word 1) words;
  (res, Art.sum res)

let load_tree fname : tree array = 
  let chan = open_in fname in 
  let data = Marshal.from_channel chan in 
  let _ = close_in chan in 
  Array.map (fun rows -> 
    let tree = Art.create () in 
    List.iter (fun (k, v) -> Art.put tree k v) rows;
    (tree, Art.sum tree)
  ) data

type token_ids = (Art.tree * int ref)

let make_token_ids () = (Art.create (), ref 0)

let give_token_id (ids:token_ids) token = 
  let tree, ctr = ids in 
  try
    (Art.get tree token)
  with Not_found -> 
    let ctrv = !ctr in
    let _ = incr ctr in 
    let _ = Art.put tree token ctrv in 
    ctrv

let get_token_id (ids:token_ids) token = 
  let tree, _ = ids in 
  Art.get tree token

let give_token_ids (ids:token_ids) (trees:tree array) = 
  Array.iter (fun (tt, _) ->
    Art.iter tt (fun k _v -> 
      let _ = give_token_id ids k in ()
    )
  ) trees

let write_token_ids (ids, _) outfname =   
  with_out outfname (fun fd -> 
    Art.iter ids (fun k v -> 
      output_string fd k;
      output_string fd "\t";
      output_string fd (string_of_int v);
      output_string fd "\n";
    );
  ); ()

let tree_similarity a b =
  let a, a_sum = a in
  let b, b_sum = b in 
  Art.fold a (fun k v res -> 
    try (
      let vb = Art.get b k in
      res - abs (v - vb)
    ) with Not_found -> res
  ) (a_sum + b_sum)



let words_vec hists words = 
  let wt = into_tree words in 
  let sims = Array.map (tree_similarity wt) hists in 
  let total_sim = float_of_int (Array.fold_left (+) 0 sims) in 
  Array.map (fun v -> (float_of_int v) /. total_sim) sims

let into_dict vs = 
  let res = Hashtbl.create (List.length vs) in 
  let _ = List.iter (fun v -> let _ = (try Hashtbl.replace res v ((Hashtbl.find res v) + 1) with Not_found -> Hashtbl.replace res v 1) in ()) vs in 
  res