open Utils
open Bigarray


let write_urls urls outf = 
  Array.iter (fun v -> 
    output_string outf (Printf.sprintf "%s\n" v);
  ) urls; ()


let write_pairs pairs outfname = 
  let len = Hashtbl.length pairs in 
  let size_bytes = len * 3 * (kind_size_in_bytes Int64) in 
  let _ = Printf.printf "%d " size_bytes in
  let _ = print_endline outfname in
  let target_fd = Unix.openfile outfname [Unix.O_RDWR; O_CREAT; O_APPEND] 0o640 in
  let _ = Unix.ftruncate target_fd size_bytes in 
  let target = Unix.map_file target_fd Int64 C_layout true [| len; 3 |] in 
  let target = array2_of_genarray target in
  let idx = ref 0 in 
  Hashtbl.iter (fun k v ->
    let l, r = k in 
    let idxv = !idx in (
      Array2.set target idxv 0 (Int64.of_int l);
      Array2.set target idxv 1 (Int64.of_int r);
      Array2.set target idxv 2 (Int64.of_int v);
    );
    incr idx;
  ) pairs;
  Unix.close target_fd; ()

let () =
  let itunes_inp_glob = Array.get Sys.argv 1 in 
  let soundcloud_inp_glob = Array.get Sys.argv 2 in 
  let podbean_channel_inp_glob = Array.get Sys.argv 3 in 
  let podbean_inp_glob = Array.get Sys.argv 4 in 
  let outp_urls_fname = Array.get Sys.argv 5 in 
  let outp_pairs_fname = Array.get Sys.argv 6 in 
  let test_map = Hashtbl.create 1 in 
  print_endline outp_pairs_fname;
  Hashtbl.add test_map (1, 1) 1;
  write_pairs test_map outp_pairs_fname;
  with_out outp_urls_fname (fun outp_urls_fd ->
      let pairs = Hashtbl.create 100000 in 
      let urls, itunes_pairs = Itunes.hash_pairs itunes_inp_glob in 
      let _ = print_endline "itunes" in 
      let _ = table_into_table (+) pairs itunes_pairs in 
      write_urls urls outp_urls_fd;
      let urls, soundcloud_pairs = Soundcloud.hash_pairs soundcloud_inp_glob in 
      let _ = print_endline "soundcloud" in 
      let _ = table_into_table (+) pairs soundcloud_pairs in 
      write_urls urls outp_urls_fd;
      let rss_mapping = Podbean.get_rss_url_mapping podbean_channel_inp_glob in 
      print_endline (Printf.sprintf "%d" (Hashtbl.length rss_mapping));
      let urls, podbean_pairs = Podbean.hash_pairs rss_mapping podbean_inp_glob in 
      let _ = print_endline "podbean" in 
      let _ = table_into_table (+) pairs podbean_pairs in 
      write_urls urls outp_urls_fd;
      write_pairs pairs outp_pairs_fname
  )
