open Utils


type ('input, 'acc) mappercombiner_result = [`End_of_stream of 'acc | `Stream_item of 'input ]

let decode s =
  Marshal.from_string s 0

let encode v =
  Marshal.to_string v []

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
  | None -> (push (`End_of_stream state)); print_endline "done";
  | Some(v) -> 
    try
        (let new_state, item = combiner state v in 
        (match item with | Some(v) -> (push (`Stream_item v)) | None -> ());
        _combiner_worker combiner new_state push pull)
    with exn -> (
        print_endline (Printexc.to_string exn);
        _combiner_worker combiner state push pull)

let streamerreducer_from_streamer_and_reducer streamer reducer =
  fun acc v ->
    match v with 
    | `End_of_stream(item) -> reducer acc item 
    | `Stream_item(item) -> ((streamer item); acc)

let streamerreducer_no_stream reducer =
  streamerreducer_from_streamer_and_reducer (fun _ -> ()) reducer

let with_context f = 
  let ctx = Zmq.Context.create () in 
  try_finalize (fun () -> f ctx) (fun () -> Zmq.Context.terminate ctx)

let with_socket ctx typ url f = 
  let ztyp = match typ with 
    | `Input_send -> Zmq.Socket.push 
    | `Input_recv -> Zmq.Socket.pull
    | `Output_send -> Zmq.Socket.push 
    | `Output_recv -> Zmq.Socket.pull in 
  let sock = Zmq.Socket.create ctx ztyp in
  (match typ with 
  | `Input_send | `Output_recv -> Zmq.Socket.bind sock url; 
  | `Input_recv | `Output_send -> Zmq.Socket.connect sock url;
  );
  (*(match typ with 
  | `Input_recv | `Output_recv -> Zmq.Socket.set_send_high_water_mark sock 10;
  | `Input_send | `Output_send -> ()
  );*)
  try_finalize (fun () -> f sock) (fun () -> Zmq.Socket.close sock)

let combiner_worker combiner initial_state input_socket_url output_socket_url =
  with_context (fun socket_ctx -> 
    with_socket socket_ctx `Input_recv input_socket_url (fun inp_socket -> 
      with_socket socket_ctx `Output_send output_socket_url (fun outp_socket ->
        let pull () = decode (Zmq.Socket.recv inp_socket) in 
        let push v = Zmq.Socket.send outp_socket (encode v) in 
        (_combiner_worker combiner initial_state push pull); ()
      )
    )
  )

let run_fork thunk = 
  if Unix.fork () == 0 then
    try_finalize thunk (fun () -> ignore (exit 0))

let run_all_cores thunk = 
  let n_cores = Nativeint.to_int (Corecount.count ()) in
  for _ = 0 to n_cores do 
    run_fork thunk;
  done;
  n_cores

let rec _generate send generator =
  match generator with 
  | Seq.Nil -> send None;
  | Seq.Cons(v, rst) -> (
    send (Some v);
    _generate send (rst ());
  )

let generate generator socket_url n_workers = 
  with_context (fun socket_ctx -> 
    with_socket socket_ctx `Input_send socket_url (fun socket -> 
      _generate (fun v -> Zmq.Socket.send socket (encode v)) (generator ());
      let none_marshal = encode None in 
      for _ = 0 to n_workers do Zmq.Socket.send socket none_marshal; done
    )
  )

let rec _fork_reduce  socket reducer done_counter acc =
  if done_counter > 0 then (
    Printf.printf "%d\n" done_counter;
    let inp = decode (Zmq.Socket.recv socket) in
    let done_counter = match inp with | `Stream_item(_) -> done_counter | `End_of_stream(_) -> done_counter - 1 in 
    _fork_reduce socket reducer done_counter (reducer acc inp)
  ) else acc

let fork_reduce reducer initial_state socket_url n_workers =
  with_context (fun socket_ctx -> 
    with_socket socket_ctx `Output_recv socket_url (fun socket -> 
      _fork_reduce socket reducer n_workers initial_state
    )
  )

let fork_combine ~generator ~combiner ~combiner_initial_state ~reducer ~reducer_initial_state =
  let pid = Unix.getpid () in 
  let inp_socket_fname = Printf.sprintf "/tmp/ocaml_combine_inp_%d" pid in 
  let outp_socket_fname = Printf.sprintf "/tmp/ocaml_combine_outp_%d" pid in
  let inp_socket_url = Printf.sprintf "ipc://%s" inp_socket_fname in 
  let outp_socket_url = Printf.sprintf "ipc://%s" outp_socket_fname in 
  let n_workers = run_all_cores (fun () -> combiner_worker combiner combiner_initial_state inp_socket_url outp_socket_url) in
  run_fork (fun () -> generate generator inp_socket_url n_workers);
  fork_reduce reducer reducer_initial_state outp_socket_url n_workers
