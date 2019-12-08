open Format

let module_fuel i = (i / 3) - 2

let filter_zero = List.filter ((<) 0)

(** Tail recursive sum *)
let sum xs =
  let rec go acc = function
      [] -> acc
    | x :: xs ->  go (acc + x) xs
  in
  go 0 xs

(** Stack recursive total fuel function *)
let rec total_fuel = function
    [] -> 0
  | xs ->
     let fs = filter_zero @@ List.map module_fuel xs in
     sum fs + total_fuel fs

let () =
  let data = ref [] in
  (
    try
      while true; do
        data := read_int () :: !data
      done
    with End_of_file -> ()
  );
  printf "%d\n" (total_fuel !data)
