open Utils


let write_urls urls outf = 
  Array.iter (fun v -> 
    output_string outf (Printf.sprintf "%s\n" v);
  ) urls; ()


let write_pairs pairs outf = 
  Hashtbl.iter (fun k v -> 
    let l, r = k in 
    output_binary_int outf l;
    output_binary_int outf (l lsr 32);
    output_binary_int outf r;
    output_binary_int outf (r lsr 32);
    output_binary_int outf v;
    output_binary_int outf (v lsr 32);
  ) pairs; ()

let () =
  let itunes_inp_glob = Array.get Sys.argv 1 in 
  let soundcloud_inp_glob = Array.get Sys.argv 2 in 
  let podbean_channel_inp_glob = Array.get Sys.argv 3 in 
  let podbean_inp_glob = Array.get Sys.argv 4 in 
  let outp_urls_fname = Array.get Sys.argv 5 in 
  let outp_pairs_fname = Array.get Sys.argv 6 in 
  with_out outp_urls_fname (fun outp_urls_fd ->
    with_out outp_pairs_fname (fun outp_pairs_fd -> 
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
      let urls, podbean_pairs = Podbean.hash_pairs rss_mapping podbean_inp_glob in 
      let _ = print_endline "podbean" in 
      let _ = table_into_table (+) pairs podbean_pairs in 
      write_urls urls outp_urls_fd;
      write_pairs pairs outp_pairs_fd
    )
  )
