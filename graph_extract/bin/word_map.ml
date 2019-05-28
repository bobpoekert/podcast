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

let make_dist_array (big_tree, big_sum) n_res dist = 
  let total = List.fold_left (fun acc (_k, v) -> acc + v) 0 dist |> float_of_int in 
  let big_sum = float_of_int big_sum in 
  let res_keys = Array.make n_res 0 in 
  let res_probs = Array.make n_res 0.0 in 
  let _ = List.iter (fun (k, v) -> 
    let prob = (float_of_int v) /. total in 
    let tot_prob = (float_of_int (Art.get big_tree k)) /. big_sum in 
    let cond_prob = prob /. tot_prob in 
    if cond_prob > (Array.get res_probs 0) then (
      let insert_idx = binary_search_idx_v res_probs cond_prob in ( 
      array_shift_right res_probs insert_idx 1;
      Array.set res_probs insert_idx cond_prob;
      array_shift_right res_keys insert_idx 1;
      Array.set res_keys insert_idx (Murmur.murmur_hash k)
      );
    )
  ) dist in
  let sort_idxes = argsort res_keys in 
  (get_indexes res_keys sort_idxes, get_indexes res_probs sort_idxes)

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

let array_map2 thunk a1 a2 = 
  Array.mapi (fun i v -> thunk v (Array.get a2 i)) a1

let calc_point x y dists_2d dists_arrays = 
  let n_dists = Array2.dim1 dists_2d in 
  let weights = Array.make n_dists 0.0 in (
    for i = 0 to (n_dists - 1) do 
      let dist_x = Array2.get dists_2d i 0 in 
      let dist_y = Array2.get dists_2d i 1 in 
      Array.set weights i (distance x y dist_x dist_y);
    done;

    let _score, k = fold_pair_arrays (fun (res_score, res_k) k probs idxes -> 
      let cnt, score = Array.fold_left (fun (i, score) prob -> 
        let idx = Array.get idxes i in 
        (i + 1, score +. (prob *. (Array.get weights idx)))
      ) (0, 0.0) probs in
      let score = score /. (float_of_int cnt) in 
      if score > res_score then (score, k) else (res_score, res_k)
    ) dists_arrays (0.0, 0) in k

  )

let point_scaler min max target = 
  let factor = (max -. min) /. target in 
  fun x -> factor *. x

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
  let d = Array2.dim1 arr in 
  if d < 1 then raise (Invalid_argument "empty array") else
  _bounds_array2 arr max_float min_float max_float min_float d 

let scalers arr target_x target_y = 
  let min_x, max_x, min_y, max_y = bounds_array2 arr in 
  (point_scaler min_x max_x target_x, point_scaler min_y max_y target_y)

let make_word_map dists_fname fname_2d outfname out_width out_height = 
  let dists = load_marshal dists_fname in 
  let n_dists = Array.length dists in 
  let big_tree = into_big_tree dists in 
  let big_tree = (big_tree, Art.sum big_tree) in 
  let dists_2d = load_array2 fname_2d Float32 n_dists in
  let scale_x, scale_y = scalers dists_2d (float_of_int out_width) (float_of_int out_height) in 
  let ncores = (Corecount.count () |> Nativeint.to_int) in
  let chunksize_x = out_width / ncores in 
  let chunksize_y = out_height / ncores in
  let dists_arrays = Array.map (make_dist_array big_tree 10000) dists in 
  array2_with_file outfname Int64 out_width out_height (fun out -> 
    parrun (fun i -> 
      let start_x = chunksize_x * i in 
      let start_y = chunksize_y * i in 
      for x = start_x to (start_x + chunksize_x) do 
        for y = start_y to (start_y + chunksize_y) do 
          Array2.set out x y (Int64.of_int (calc_point (scale_x (float_of_int x)) (scale_y (float_of_int y)) dists_2d dists_arrays));
          print_endline "."
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
