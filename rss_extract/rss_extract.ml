open Lib_rss_extract


let () =
  let argc = (Array.length Sys.argv) - 1 in 
  let outfname = Sys.argv.(1) in
  let hist_basename = Sys.argv.(2) in
  let n_infiles = argc - 2 in
  let n_cores = Nativeint.to_int (Corecount.count ()) in 
  let n_cores = if n_cores > n_infiles then n_infiles else n_cores in 
  let partition_size = n_infiles / n_cores in
  let hists = Histogram.load hist_basename in 
  let pid_mapper part_idx =
    let part_start = 3 + part_idx * partition_size in 
    let _ = Printf.printf "part start: %d %d %d\n" argc part_start partition_size in 
    let part = Array.sub Sys.argv part_start partition_size in 
    let pid = Unix.fork () in 
    if pid == 0 then
      let _ = (
        try process_batch hists (Printf.sprintf "%s.%d" outfname part_idx) part
        with _ -> exit 0
      ) in 0
    else pid in 
  let pids = List.map pid_mapper (range n_cores) in 
  List.iter (fun pid -> let _ = (Unix.waitpid [] pid) in ()) pids;
  exit 0;