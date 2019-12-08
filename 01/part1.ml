open Format

let calculate_mass i = (i / 3) - 2

let rec sum = function
    [] -> 0
  | x :: xs -> x + sum xs
  
let () =
  let data = ref [] in
  (
    try
      while true; do
        data := read_int () :: !data
      done
    with End_of_file -> ()
  );
  printf "%d\n" (sum (List.map calculate_mass !data))
