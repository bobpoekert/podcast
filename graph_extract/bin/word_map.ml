open Bigarray
open Utils

let into_big_tree items_array = 
  let res = Art.create () in 
  let _ = Array.iter (fun items -> 
    List.iter (fun (k, v) -> 
      Art.incr res k v;
    ) items; ()
  ) items_array in 
  res

let array_shift_right arr idx n = 
  Array.blit arr idx arr (idx + n) ((Array.length arr) - idx - n)

let assert_ordered ?(reverse=false) arr = 
  if (Array.length arr) > 1 then 
    let _ = Array.fold_left (fun prev v -> assert (if reverse then (prev >= v) else (v >= prev)); v) (Array.get arr 0) arr in ()

let make_dist_array (big_tree, big_sum) n_res p_dist dist = 
  let list_len = List.length dist in 
  let n_res = min n_res list_len in 
  let total = List.fold_left (fun acc (_k, v) -> acc + v) 0 dist |> float_of_int in 
  let big_sum = float_of_int big_sum in 
  let res_keys = Array.make list_len 0 in 
  let res_probs = Array.make list_len 0.0 in 
  let res_size = List.fold_left (fun i (k, v) -> 
    let k_hash = Murmur.murmur_hash k in 
    let prob = (float_of_int v) /. total in 
    let tot_prob = (float_of_int (Art.get big_tree k)) /. big_sum in 
    let cond_prob = (prob *. p_dist) /. tot_prob in 
    assert (tot_prob >= 0.0 && tot_prob <= 1.0);
    assert (prob >= 0.0 && prob <= 1.0);
    assert (tot_prob >= 0.0 && tot_prob <= 1.0);
    if cond_prob > 0.0001 then (
        Array.set res_keys i k_hash;
        Array.set res_probs i cond_prob;
        i + 1
    ) else i
  ) 0 dist in
  if res_size < 1 then (Array.make 0 0, Array.make 0 0.0) else (
      let res_keys = Array.sub res_keys 0 res_size in 
      let res_probs = Array.sub res_probs 0 res_size in 
      let n_res = min n_res res_size in
      let prob_sort_idxes = argsort_generic (fun a b -> let v = a -. b in if v == 0.0 then 0 else if v < 0.0 then 1 else -1) res_probs in 
      let res_keys = get_indexes res_keys prob_sort_idxes in 
      let res_keys = Array.sub res_keys 0 n_res in 
      let res_probs = get_indexes res_probs prob_sort_idxes in 
      let res_probs = Array.map log res_probs in
      let _ = assert_ordered ~reverse:true res_probs in 
      let res_probs = Array.sub res_probs 0 n_res in 
      let sort_idxes = argsort res_keys in
      let ks = get_indexes res_keys sort_idxes in 
      let vs = get_indexes res_probs sort_idxes in 
      let _ = assert_ordered ks in 
      (ks, vs)
  )

let distance x1 y1 x2 y2 = 
  let dx = x2 -. x1 in 
  let dy = y2 -. y1 in 
  sqrt ((dx *. dx) +. (dy *. dy))

let fold_pair_arrays thunk (arrs: (int array * 'a array) array) init = 
  (* k-way merge *)
  let insize = Array.length arrs in 
  let arg_idxes = Array.make insize 0 in
  let arg = Array.make insize (Array.get (let _, v = (Array.get arrs 0) in v) 1) in
  let idxes = Array.make insize 0 in 
  let lengths = Array.map (fun (k, _) -> Array.length k) arrs in 
  let rec _fold_pair_arrays acc n_arrays =
    if n_arrays < 1 then acc else (
      let _, mink = Array.fold_left (fun (i, mink) idx -> 
        let ks, _vs = Array.get arrs i in 
        (i + 1, if idx >= (Array.get lengths i) then mink else min (Array.get ks idx) mink)
      ) (0, max_int) idxes in 
      let _, argsize, n_arrays = Array.fold_left (fun (i, argsize, n_arrays) idx -> 
        let ks, vs = Array.get arrs i in 
        if idx >= (Array.get lengths i) then (i + 1, argsize, n_arrays) else (
          let k = Array.get ks idx in 
          let n_arrays = if (idx + 1) >= (Array.get lengths i) then (n_arrays - 1) else n_arrays in 
          if k == mink then (
            Array.set arg argsize (Array.get vs idx);
            Array.set idxes i (idx + 1);
            Array.set arg_idxes argsize i;
            (i + 1, argsize + 1, n_arrays)
          ) else (i + 1, argsize, n_arrays)
        )
      ) (0, 0, n_arrays) idxes in
      _fold_pair_arrays (thunk acc mink (Array.sub arg 0 argsize) (Array.sub arg_idxes 0 argsize)) n_arrays
    ) in 
  _fold_pair_arrays init insize

let pivot_pair_arrays arrs = 
  fold_pair_arrays (fun res k vs idxes -> (k, vs, idxes) :: res) arrs []

let array_map2 thunk a1 a2 = 
  Array.mapi (fun i v -> thunk v (Array.get a2 i)) a1

let tree_to_arrays (tree, tree_sum) = 
  let tree_sum = float_of_int tree_sum in 
  let size = Art.length tree in 
  let hashes = Array.make size 0 in 
  let probs = Array.make size 0.0 in 
  let _ = Art.fold tree (fun k count idx -> 
    let hash = Murmur.murmur_hash k in 
    let prob = (float_of_int count) /. tree_sum in (
      assert (prob >= 0.0 && prob <= 1.0);
      Array.set hashes idx hash;
      Array.set probs idx prob;
      idx + 1
    )
  ) 0 in 
  let sort_idxes = argsort hashes in 
  (get_indexes hashes sort_idxes, get_indexes probs sort_idxes)


let calc_point x y dists_2d dists_arrays (_big_hashes, _big_probs) = 
  let n_dists = Array2.dim1 dists_2d in 
  let weights = Array.make n_dists 0.0 in (
    for i = 0 to (n_dists - 1) do 
      let dist_x = Array2.get dists_2d i 0 in 
      let dist_y = Array2.get dists_2d i 1 in 
      Array.set weights i (distance x y dist_x dist_y);
    done;
    let weight_sum = Array.fold_left (+.) 0.0 weights in
    for i = 0 to (n_dists - 1) do 
      Array.set weights i ((Array.get weights i) /. weight_sum);
    done;
    let norm = (Array.fold_left (+.) 0.0 weights) in
    assert ((norm -. 1.0) < 0.00000001);

    let _score, k = List.fold_left (fun (res_score, res_k) (k, probs, idxes) -> 
      let cnt, score = Array.fold_left (fun (i, score) prob -> 
        let idx = Array.get idxes i in 
        let weight = Array.get weights idx in 
        (i + 1, score +. (prob /. weight))
      ) (0, 0.0) probs in
      let score = score /. (float_of_int cnt) in
      if score >= res_score then (score, k) else (res_score, res_k)
    ) (0.0, 0) dists_arrays  in k

    (* let _ = Printf.printf "%f %d " score k in k *)
  )

let point_scaler min max target = 
(*

y - y1 = ((y2 - y1) / (x2 - x1)) * (x - x1)
x = x
y1 = min
y2 = max
x1 = 0
x2 = target

y - min = ((max - min) / target) * x
y = ((max - min) / target) * x + min

*)
  let factor = (max -. min) /. target in 
  fun x -> (factor *. x) +. min

let rec _bounds_array2 arr min_x max_x min_y max_y row = 
  if row < 0 then
    (min_x, max_x, min_y, max_y)
  else
    let x = Array2.get arr row 0 in 
    let y = Array2.get arr row 1 in 
    _bounds_array2 arr 
      (min min_x x)
      (max max_x x)
      (min min_y y)
      (max max_y y)
      (row - 1)

let bounds_array2 arr = 
  let d = (Array2.dim1 arr) - 1 in 
  if d < 0 then raise (Invalid_argument "empty array") else
  _bounds_array2 arr max_float min_float max_float min_float d 

let scalers arr target_x target_y = 
  let min_x, max_x, min_y, max_y = bounds_array2 arr in 
  let _ = Printf.printf "%f %f %f %f" min_x min_y max_x max_y in 
  let _ = print_endline "" in 
  (point_scaler min_x max_x target_x, point_scaler min_y max_y target_y)

let make_word_map dists_fname fname_2d outfname out_width out_height = 
  let dists = load_marshal dists_fname in 
  let n_dists = Array.length dists in 
  let big_tree = into_big_tree dists in 
  let big_tree = (big_tree, Art.sum big_tree) in 
  let big_arr = tree_to_arrays big_tree in 
  let dists_2d = load_array2 fname_2d Float32 n_dists in
  let scale_x, scale_y = scalers dists_2d (float_of_int out_width) (float_of_int out_height) in 
  let ncores = (Corecount.count () |> Nativeint.to_int) in
  let chunksize_x = out_width / ncores in
  let p_dist = 1.0 /. (float_of_int (Array.length dists)) in 
  let dists_arrays = Array.map (make_dist_array big_tree 5000 p_dist) dists in 
  let _ = Array.iter (fun (k, _v) -> Printf.printf "%d " (Array.length k)) dists_arrays in
  let _ = print_endline "" in 
  let dists_arrays = pivot_pair_arrays dists_arrays in
  array2_with_file outfname Int64 out_width out_height (fun out -> 
    parrun (fun i -> 
      let start_x = (chunksize_x * i) + 1 in 
      for x = start_x to (start_x + chunksize_x - 1) do 
        for y = 0 to (out_height - 1) do 
          Array2.set out x y (Int64.of_int (calc_point (scale_x (float_of_int x)) (scale_y (float_of_int y)) dists_2d dists_arrays big_arr));
          (* Printf.printf "%d %d" x y;
          print_endline "" *)
        done
      done
    )
  )

let () = 
  let dists_fname = Array.get Sys.argv 1 in 
  let fname_2d = Array.get Sys.argv 2 in 
  let width = int_of_string (Array.get Sys.argv 3) in 
  let height = int_of_string (Array.get Sys.argv 4) in 
  let outfname = Array.get Sys.argv 5 in 
  make_word_map dists_fname fname_2d outfname width height
