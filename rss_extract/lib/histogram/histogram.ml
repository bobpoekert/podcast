type histogram
external load : string -> histogram array = "call_histogram_load"
external get : histogram -> string -> int = "call_histogram_get"
external sum : histogram -> int = "call_histogram_sum"