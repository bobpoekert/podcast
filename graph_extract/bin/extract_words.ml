open Utils

let () = 
  let infname = Array.get Sys.argv 1 in 
  let dists = load_marshal infname in 
  Array.iter (fun items ->
    List.iter (fun (k, _v) ->
      Printf.printf "%d\t%s\n" (Murmur.murmur_hash k) k;
    ) items;
  ) dists; ()
