open Lib_rss_extract


let () =
  let argc = (Array.length Sys.argv) - 1 in 
  let outfname = Sys.argv.(1) in
  let hist_basename = Sys.argv.(2) in
  let n_infiles = argc - 2 in
  let hists = Histogram.load hist_basename in 
  let infnames = Array.sub Sys.argv 3 n_infiles in
  process_batch hists outfname infnames