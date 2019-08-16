open Bigarray
open Utils
open Lib.Rss
open Lib.Tokens

let load_cluster_ids fname = 
  let arr = load_array1 fname Int64 in 
  let len = Array1.dim arr in 
  let hashes = Array1.sub arr 0 (len / 2) in 
  let cluster_ids = Array1.sub arr (len / 2) (len / 2) in 
  (hashes, cluster_ids)

let url_cluster_id (hashes, clusters) url = 
  let k = url_hash url |> Int64.of_int in 
  let idx = binary_search hashes k in 
  Int64.to_int (Array1.get clusters idx)

let iter_word_histograms cluster_ids word_hists fname =
  try (
      iter_xml_pages fname (fun (_req : Warc.warc_entry) (headers : Warc.header) _head xml ->
        try (
            let meta_text = List.concat (List.map tokenize_text (channel_meta_text xml)) in 
            let url = Warc.get_url headers in 
            let cluster_id = url_cluster_id cluster_ids url in 
            let cluster_hist = Array.get word_hists cluster_id in 
            List.iter (fun word -> Art.incr cluster_hist word 1) meta_text;
        ) with Not_found -> ()
      );
  ) with _ -> ()

let word_histogram_worker cluster_ids word_hists fnames = 
  (* generates a histogram over words for each topic *)
  let _ = Array.iter (iter_word_histograms cluster_ids word_hists) fnames in 
  let _ = Printf.printf "%d" (Array.fold_left (+) 0 (Array.map Art.length word_hists)) in 
  let _ = print_endline "" in 
  Array.map (Art.items) word_hists

let word_histogram_reducer res hists = 
  (* sum the histograms from each of the histogram workers *)
  (for i = 0 to ((Array.length res) - 1) do 
    let target = Array.get res i in 
    let src = Array.get hists i in 
    List.iter (fun (k, v) -> Art.incr target k v) src;
  done);
  res

let pairwise_tree_similarities target trees = 
  let n_trees = (Array.length trees) - 1 in
  let batch_size = n_trees / (corecount ()) in 
  parrun (fun batch ->
    let start = batch_size * batch in 
    for l = start to (start + batch_size) do 
      for r = 0 to n_trees do 
        if l != r then (
          Array2.set target l r (Int64.of_int (tree_similarity (Array.get trees l) (Array.get trees r)))
        )
      done
    done;
  );
  target

let pairwise_tree_similarities_to_file fname trees = 
  let dim_size = Array.length trees in 
  let _ = array2_with_file fname Int64 dim_size dim_size (fun arr -> pairwise_tree_similarities arr trees) in ()


let process_pages fnames clusters_fname outfname pairwise_outfname = 
  let clusters = load_cluster_ids clusters_fname in 
  let _cluster_hashes, cluster_ids = clusters in 
  let distinct_cluster_ids = Hashtbl.create 1024 in 
  for i = 0 to (Array1.dim cluster_ids) - 1 do 
    Hashtbl.replace distinct_cluster_ids (Array1.get cluster_ids i) 1;
  done;
  let n_clusters = Hashtbl.length distinct_cluster_ids in 
  let _ = Printf.printf "%d" n_clusters in 
  let _ = print_endline "" in 
  let first_hist = Art.create () in 
  (* generate word histograms for each topic *)
  let hists = Array.make n_clusters first_hist in 
  Array.set hists 0 first_hist;
  for i = 1 to (n_clusters - 1) do 
    Array.set hists i (Art.create ())
  done;
  let res_word_histograms = parmap fnames (word_histogram_worker clusters hists) word_histogram_reducer hists in 
  let res_word_histograms = array_filteri (fun _i v -> (Art.length v) > 1) res_word_histograms in 
  let res_trees = Array.map (fun t -> (t, Art.sum t)) res_word_histograms in 
  let out = open_out outfname in
  Marshal.to_channel out (Array.map Art.items res_word_histograms) [];
  close_out out;
  (* generate topic x topic similarity matrix *)
  pairwise_tree_similarities_to_file pairwise_outfname res_trees; ()

let () = 
  let clusters_fname = Array.get Sys.argv 1 in 
  let outfname = Array.get Sys.argv 2 in 
  let pairwise_outfname = Array.get Sys.argv 3 in
  let fnames = Array.sub Sys.argv 4 ((Array.length Sys.argv) - 4) in 
  process_pages fnames clusters_fname outfname pairwise_outfname; ()
