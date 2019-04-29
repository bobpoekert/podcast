
let () =
  let inp_glob = Array.get Sys.argv 1 in 
  let outp_urls_fname = Array.get Sys.argv 2 in 
  let outp_pairs_fname = Array.get Sys.argv 3 in 
  let outp_urls_fd = open_out outp_urls_fname in 
  let outp_pairs_fd = open_out outp_pairs_fname in 
  let urls, pairs = Itunes.sha_hash_pairs inp_glob in 
  Array.iter (fun v -> 
    output_string outp_urls_fd (Printf.sprintf "%s\n" v);
  ) urls;
  close_out outp_urls_fd;
  Hashtbl.iter (fun k v -> 
    let l, r = k in 
    output_string outp_pairs_fd l;
    output_string outp_pairs_fd r;
    output_binary_int outp_pairs_fd v;
  ) pairs;
  close_out outp_pairs_fd;