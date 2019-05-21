
type tree

val create : unit -> tree
val put : tree -> string -> int -> unit 
val get : tree -> string -> int 
val length : tree -> int
val update : tree -> string -> (int -> int) -> unit
val remove : tree -> string -> bool
val iter : tree -> (string -> int -> unit) -> unit 
val fold : tree -> (string -> int -> 'b -> 'b) -> 'b -> 'b
val iter_prefix : tree -> string -> (string -> int -> unit) -> unit
val fold_prefix : tree -> string -> (string -> int -> 'b -> 'b) -> 'b -> 'b
val sum_prefix : tree -> string -> int
val merge : tree -> tree -> (int -> int -> int) -> tree
val sum : tree -> int
val items : tree -> (string * int) list
val incr : tree -> string -> int -> unit
