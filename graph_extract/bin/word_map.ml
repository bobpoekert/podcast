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
    let prob = (float_of_int v) /. (total /. 8.0) in 
    let tot_prob = (float_of_int (Art.get big_tree k)) /. big_sum in 
    let cond_prob = (prob *. p_dist) /. tot_prob in 
    (*assert (tot_prob >= 0.0 && tot_prob <= 1.0);
    assert (prob >= 0.0 && prob <= 1.0);
    assert (tot_prob >= 0.0 && tot_prob <= 1.0); *)
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
  if (Array.length arrs) < 1 then init else (
    let insize = Array.length arrs in 
    let arg_idxes = Array.make insize 0 in
    let arg = Array.make insize (Array.get (snd (Array.get arrs 0)) 1) in
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
  )

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

let k_neighbors = 5

type word_prob = (int * float)
module WordProbCompare = struct
  type t = word_prob
  let compare (a:word_prob) (b:word_prob) = compare (snd a) (snd b)
end

module WordHeap = BatHeap.Make(WordProbCompare)
type word_heap = WordHeap.t

let k_insert heap v k = 
  let heap = WordHeap.insert heap v in 
  let size = WordHeap.size heap in 
  if size > k then 
    WordHeap.del_min heap 
  else 
    heap

let heap_to_arrays (heap:word_heap) =
  let size = WordHeap.size heap in 
  let ks = Array.make size 0 in 
  let vs = Array.make size 0.0 in (
    BatEnum.iteri (fun idx (k, v) -> 
      Array.set ks idx k;
      Array.set vs idx v;
    ) (WordHeap.enum heap);
    (ks, vs)
  )


let max_res_words = 100

let calc_point x y dists_2d dists_arrays  = 
  let n_dists = Array2.dim1 dists_2d in 
  let weights = Array.make n_dists 0.0 in (
    let _ = for i = 0 to (n_dists - 1) do 
      let dist_x = Array2.get dists_2d i 0 in 
      let dist_y = Array2.get dists_2d i 1 in 
      Array.set weights i (distance x y dist_x dist_y);
    done in 
    let k_neighbors = min n_dists k_neighbors in 
    let top_k_dists = Array.make k_neighbors (Array.get dists_arrays 0) in 
    let top_k_weights = Array.make k_neighbors 0.0 in 
    let weight_idxes = argsort weights in 
    let _ = for i = 0 to (k_neighbors - 1) do 
      let widx = Array.get weight_idxes i in 
      Array.set top_k_dists i (Array.get dists_arrays widx);
      Array.set top_k_weights i (Array.get weights widx);
    done in
    let weight_sum = Array.fold_left (+.) 0.0 weights in
    let _ = for i = 0 to (n_dists - 1) do 
      Array.set weights i ((Array.get weights i) /. weight_sum);
    done in 
    let heap = fold_pair_arrays (fun res k probs idxes -> 
      let score = ref 0.0 in 
      let n_res = Array.length probs in (
        for i = 0 to n_res - 1 do 
          let idx = Array.get idxes i in 
          let weight = Array.get top_k_weights idx in 
          let prob = Array.get probs i in 
          score := !score +. (prob /. weight)
        done;
        let score = !score in 
        k_insert res (k, score) 100
      )
    ) top_k_dists WordHeap.empty in
    let ks, vs = heap_to_arrays heap in
    let idxes = argsort ks in 
    (get_indexes ks idxes, get_indexes vs idxes)

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

let set_array_pairs out x y (ks, vs) = 
  let n_items = Array.length ks in 
  for i = 0 to (n_items - 1) do 
    Genarray.set out [| x; y; i; 0 |] (Int64.of_int (Array.get ks i));
    Genarray.set out [| x; y; i; 1 |] (Int64.bits_of_float (Array.get vs i));
  done

let topics_in_bbox dists_2d min_x min_y max_x max_y = 
  let max_len = Array2.dim1 dists_2d in 
  let res = Array.make max_len 0 in 
  let rec populate_idxes res_idx inp_idx = 
    if inp_idx >= max_len then res_idx else (
      let v_x = Array2.get dists_2d inp_idx 0 in 
      let v_y = Array2.get dists_2d inp_idx 1 in 
      if v_x >= min_x && v_y >= min_y && v_x <= max_x && v_y <= max_y then (
        Array.set res res_idx inp_idx;
        populate_idxes (res_idx + 1) (inp_idx + 1)
      ) else populate_idxes res_idx (inp_idx + 1)
    ) in 
  let res_len = populate_idxes 0 0 in 
  Array.sub res 0 res_len

let make_word_map dists_fname fname_2d outfname out_width out_height min_x min_y max_x max_y is_parallel = 
  let dists = load_marshal dists_fname in 
  let n_dists = Array.length dists in 
  let dists = Array.map (fun terms -> List.filter (fun (_k, count) -> count > 10) terms) dists in
  let dists_idxes = argfilter (fun l -> (List.length l) > 0) dists in 
  let dists = Array.map (Array.get dists) dists_idxes in 
  let dists_2d = load_array2 fname_2d Float32 n_dists in
  let dists_2d = array2_slice_rows dists_2d dists_idxes in 
  let inner_scale_x, inner_scale_y = scalers dists_2d (float_of_int out_width) (float_of_int out_height) in 
  let big_min_x, big_max_x, big_min_y, big_max_y = bounds_array2 dists_2d in
  let downscale_x = (big_max_x -. big_min_x) /. (max_x -. min_x) in 
  let scale_x x = (inner_scale_x x) *. downscale_x in 
  let downscale_y = (big_max_y -. big_min_y) /. (max_y -. min_y) in 
  let scale_y y = (inner_scale_y y) *. downscale_y in 
  let bbox_mask = topics_in_bbox dists_2d min_x min_y max_x max_y in 
  let _ = Printf.printf "%d\n" (Array.length bbox_mask) in
  let dists_2d = array2_slice_rows dists_2d bbox_mask in 
  let dists = Array.map (fun idx -> Array.get dists idx) bbox_mask in 
  let big_tree = into_big_tree dists in 
  let big_tree = (big_tree, Art.sum big_tree) in
  let ncores = (Corecount.count () |> Nativeint.to_int) in
  let chunksize_x = out_width / ncores in
  let p_dist = 1.0 /. (float_of_int (Array.length dists)) in 
  let dists_arrays = Array.map (make_dist_array big_tree 10000 p_dist) dists in 
  let _ = Array.iter (fun (k, _v) -> Printf.printf "%d " (Array.length k)) dists_arrays in
  let _ = print_endline "" in 
  genarray_with_file outfname Int64 [| out_width; out_height; max_res_words; 2 |] (fun out -> 
    let render start_x end_x = (
      for x = start_x to end_x do 
        for y = 0 to (out_height - 1) do 
          try
              let res = (calc_point (scale_x (float_of_int x)) (scale_y (float_of_int y)) dists_2d dists_arrays) in 
              set_array_pairs out x y res
          with Invalid_argument _ -> ()
        done
      done
    ) in
    if is_parallel then 
      parrun (fun i -> 
        let start_x = chunksize_x * i in 
        render start_x (start_x + chunksize_x - 1)
      ) else render 0 (out_width - 1)
    )

let () = 
  let dists_fname = Array.get Sys.argv 1 in 
  let fname_2d = Array.get Sys.argv 2 in 
  let width = int_of_string (Array.get Sys.argv 3) in 
  let height = int_of_string (Array.get Sys.argv 4) in 
  let outfname = Array.get Sys.argv 5 in 
  let min_x = float_of_string (Array.get Sys.argv 6) in 
  let min_y = float_of_string (Array.get Sys.argv 7) in 
  let max_x = float_of_string (Array.get Sys.argv 8) in 
  let max_y = float_of_string (Array.get Sys.argv 9) in 
  let is_parallel = ((Array.get Sys.argv 10) = "true") in 
  make_word_map dists_fname fname_2d outfname width height min_x min_y max_x max_y is_parallel
