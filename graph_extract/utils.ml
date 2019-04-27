
let gzip_files_stream basepath glob =
  Unix.open_process_in (Printf.sprintf "find %s -name %s | xargs gunzip -c" basepath glob)

let xz_files_stream basepath glob =
  Unix.open_process_in (Printf.sprintf "find %s -name %s | xargs xzcat" basepath glob)

let line_reader instream =
  (fun () -> try Some(input_line instream) with End_of_file -> None)