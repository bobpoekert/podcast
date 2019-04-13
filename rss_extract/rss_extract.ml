open Lib_rss_extract


let () =
  let argc = (Array.length Sys.argv) - 1 in 
  let outfname = Sys.argv.(1) in
  let hist_basename = Sys.argv.(2) in
  let n_hists = int_of_string Sys.argv.(3) in
  let n_infiles = argc - 3 in
  let n_cores = Nativeint.to_int (Corecount.count ()) in 
  let partition_size = n_infiles / n_cores in
  let hists = List.map (fun idx -> Histogram.load (Printf.sprintf "%s%d" hist_basename idx)) (range n_hists) in
  let pids = List.map (fun part_idx -> 
    let part_start = part_idx * partition_size in 
    let part = Array.sub Sys.argv (part_start + 3) (part_start + 3 + partition_size) in 
    let pid = Unix.fork () in 
    if pid == 0 then
      (process_batch hists (Printf.sprintf "%s%d" outfname part_idx) part;
      exit 0)
    else pid) (range n_cores) in 
  List.iter (fun pid -> let _ = (Unix.waitpid [] pid) in ()) pids;
  exit 0;