
type ('input, 'acc) mappercombiner_result = [`End_of_stream of 'acc | `Stream_item of 'input ]

let decode s =
  Marshal.from_string s 0

let encode v =
  Marshal.to_string v []

let try_finalize f finally =
  let res = try f () with exn -> finally (); raise exn in
  finally ();
  res

let mappercombiner_from_mapper_and_combiner mapper combiner =
  let res: 'a -> 'v -> ('a * 'b option) = fun acc v ->
    let res = mapper v in 
    let newacc = combiner acc res in 
    (newacc, Some res) in res

let mappercombiner_no_stream combiner =
  fun acc v ->
    let res = combiner acc v in 
    (res, None)

let rec _combiner_worker combiner state push pull =
  let inp = pull () in 
  match inp with 
  | None -> (push (`End_of_stream state)); ()
  | Some(v) -> 
    let new_state, item = combiner state v in 
    (match item with | Some(v) -> (push (`Stream_item v)) | None -> ());
    _combiner_worker combiner new_state push pull

let streamerreducer_from_streamer_and_reducer streamer reducer =
  fun acc v ->
    match v with 
    | `End_of_stream(item) -> reducer acc item 
    | `Stream_item(item) -> ((streamer item); acc)

let streamerreducer_no_stream reducer =
  streamerreducer_from_streamer_and_reducer (fun _ -> ()) reducer

let combiner_worker combiner initial_state input_socket_url output_socket_url =
  let socket_ctx = Zmq.Context.create () in 
  let inp_socket = Zmq.Socket.create socket_ctx Zmq.Socket.pull in 
  let outp_socket = Zmq.Socket.create socket_ctx Zmq.Socket.push in 
  Zmq.Socket.connect inp_socket input_socket_url;
  Zmq.Socket.connect outp_socket output_socket_url;
  Zmq.Socket.set_send_high_water_mark outp_socket 10;
  let pull () = decode (Zmq.Socket.recv inp_socket) in 
  let push v = Zmq.Socket.send outp_socket (encode v) in 
  (_combiner_worker combiner initial_state push pull); ()

let run_fork thunk = 
  if Unix.fork () == 0 then
    try_finalize thunk (fun () -> ignore (exit 0))

let run_all_cores thunk = 
  let n_cores = Nativeint.to_int (Corecount.count ()) in 
  for _ = 0 to n_cores do 
    run_fork thunk;
  done;
  n_cores

let rec _generate socket generator = 
  match generator () with 
  | Some(v) -> Zmq.Socket.send socket (encode v); _generate socket generator
  | None -> ()

let generate generator socket_url n_workers = 
  let socket_ctx = Zmq.Context.create () in 
  let socket = Zmq.Socket.create socket_ctx Zmq.Socket.push in
  Zmq.Socket.connect socket socket_url;
  Zmq.Socket.set_send_high_water_mark socket 10;
  _generate socket generator;
  let none_marshal = encode None in 
  for _ = 0 to n_workers do Zmq.Socket.send socket none_marshal; done

let rec _fork_reduce  socket reducer done_counter acc =
  if done_counter > 0 then 
    let inp = decode (Zmq.Socket.recv socket) in
    let done_counter = match inp with | `Stream_item(_) -> done_counter | `End_of_stream(_) -> done_counter - 1 in 
    _fork_reduce socket reducer done_counter (reducer acc inp)
  else acc

let fork_reduce reducer initial_state socket_url n_workers =
  let socket_ctx = Zmq.Context.create () in 
  let socket = Zmq.Socket.create socket_ctx Zmq.Socket.pull in
  Zmq.Socket.connect socket socket_url;
  _fork_reduce socket reducer n_workers initial_state

let fork_combine ~generator ~combiner ~combiner_initial_state ~reducer ~reducer_initial_state =
  let pid = Unix.getpid () in 
  let inp_socket_fname = Printf.sprintf "/tmp/ocaml_combine_inp_%d" pid in 
  let outp_socket_fname = Printf.sprintf "/tmp/ocaml_combine_outp_%d" pid in
  let inp_socket_url = Printf.sprintf "ipc://%s" inp_socket_fname in 
  let outp_socket_url = Printf.sprintf "ipc://%s" outp_socket_fname in 
  let n_workers = run_all_cores (fun () -> combiner_worker combiner combiner_initial_state inp_socket_url outp_socket_url) in
  run_fork (fun () -> generate generator inp_socket_url n_workers);
  fork_reduce reducer reducer_initial_state outp_socket_url n_workers