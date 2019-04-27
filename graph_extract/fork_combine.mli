type ('input, 'acc) mappercombiner_result

val fork_combine :
  generator:(unit -> 'input option) ->
  combiner:('comb_acc -> 'input -> ('comb_acc * 'output_item option)) -> 
  combiner_initial_state:'comb_acc ->
  reducer:('reduce_acc -> ('output_item, 'comb_acc) mappercombiner_result -> 'reduce_acc) -> 
  reducer_initial_state:'reduce_acc ->
  'reduce_acc

val mappercombiner_from_mapper_and_combiner : ('input -> 'output_item) -> ('acc -> 'output_item -> 'acc) -> ('acc -> 'input -> ('acc * 'output_item option))
val mappercombiner_no_stream : ('acc -> 'input -> 'acc) -> ('acc -> 'input -> ('acc * 'a option))

val streamerreducer_from_streamer_and_reducer : ('item -> unit) -> ('r_acc -> 'c_acc -> 'r_acc) -> ('r_acc -> ('item, 'c_acc) mappercombiner_result -> 'r_acc)
val streamerreducer_no_stream : ('r_acc -> 'c_acc -> 'r_acc) -> ('r_acc -> ('a, 'c_acc) mappercombiner_result -> 'r_acc)