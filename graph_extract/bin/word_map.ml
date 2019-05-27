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
  let idxes = Array.make insize 0 in
  let arg = Array.make insize (Array.get (let _, v = (Array.get arrs 0) in v) 1) in 
  let lengths = Array.map (fun (k, _) -> Array.length k) arrs in 
  let rec _fold_pair_arrays acc =
    let _, n_arrays = Array.fold_left (fun (i, n) idx -> 
      let len = Array.get lengths i in 
      (i + 1, if idx <= len then n + 1 else n)
    ) (0, 0) idxes in 
    if n_arrays < 1 then acc else (
      let _, mink = Array.fold_left (fun (i, mink) idx -> 
        let ks, _vs = Array.get arrs i in 
        (i + 1, if idx >= (Array.get lengths i) then mink else min (Array.get ks idx) mink)
      ) (0, max_int) idxes in 
      let _, argsize = Array.fold_left (fun (i, argsize) idx -> 
        let ks, vs = Array.get arrs i in 
        if idx >= (Array.get lengths i) then (i + 1, argsize) else (
          let k = Array.get ks idx in 
          if k = mink then (
            Array.set arg argsize (Array.get vs idx);
            Array.set idxes i (idx + 1);
            (i + 1, argsize + 1)
          ) else (i + 1, argsize)
        )
      ) (0, 0) idxes in
      _fold_pair_arrays (thunk acc mink (Array.sub arg 0 argsize))
    ) in 
  _fold_pair_arrays init


let calc_point x y dists_2d dists_arrays = 
  let n_dists = Array2.dim1 dists_2d in 
  let weights = Array.make n_dists 0.0 in (
    for i = 0 to n_dists do 
      let dist_x = Array2.get dists_2d i 0 in 
      let dist_y = Array2.get dists_2d i 1 in 
      Array.set weights i (distance (float_of_int x) (float_of_int y) dist_x dist_y);
    done;

    let _score, k = fold_pair_arrays (fun (res_score, res_k) k probs -> 
      let cnt, score = Array.fold_left (fun (i, score) prob -> 
        (i + 1, score +. (prob *. (Array.get weights i)))
      ) (0, 0.0) probs in
      let score = score /. (float_of_int cnt) in 
      if score > res_score then (score, k) else (res_score, res_k)
    ) dists_arrays (0.0, 0) in k

  )

let make_word_map dists_fname fname_2d outfname out_width out_height = 
  let dists = load_marshal dists_fname in 
  let n_dists = Array.length dists in 
  let big_tree = into_big_tree dists in 
  let big_tree = (big_tree, Art.sum big_tree) in 
  let dists_2d = load_array2 fname_2d Float64 n_dists in 
  let ncores = (Corecount.count () |> Nativeint.to_int) in
  let chunksize_x = out_width / ncores in 
  let chunksize_y = out_height / ncores in
  let dists_arrays = Array.map (make_dist_array big_tree 1000) dists in 
  array2_with_file outfname Int64 out_width out_height (fun out -> 
    let pids = Array.make ncores 0 in 
    (for i = 0 to ncores do 
      let pid = Unix.fork () in 
      if pid == 0 then
        try_finalize (fun () ->
          let start_x = chunksize_x * i in 
          let start_y = chunksize_y * i in 
          for x = start_x to (start_x + chunksize_x) do 
            for y = start_y to (start_y + chunksize_y) do 
              Array2.set out x y (Int64.of_int (calc_point x y dists_2d dists_arrays));
            done
          done
        ) (fun () -> exit 0)
      else (
        Array.set pids i pid;
      )
    done);
    Array.iter (fun pid -> let _ = Unix.waitpid [] pid in ()) pids;
  )

let () = 
  let dists_fname = Array.get Sys.argv 1 in 
  let fname_2d = Array.get Sys.argv 2 in 
  let width = int_of_string (Array.get Sys.argv 3) in 
  let height = int_of_string (Array.get Sys.argv 4) in 
  let outfname = Array.get Sys.argv 5 in 
  make_word_map dists_fname fname_2d outfname width height