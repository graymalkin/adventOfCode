exception Invalid_opcode of int
                          
let get mem addr = Hashtbl.find mem addr

let put mem addr data = Hashtbl.add mem addr data
  
let bin f mem x y o =
  let a = get mem x in
  let b = get mem y in
  put mem o (f a b)

let rec run mem pc =
  let operation = get mem pc in
  let x = get mem (pc+1) in
  let y = get mem (pc+2) in
  let o = get mem (pc+3) in
  match operation with
  | 1 ->
     bin (+) mem x y o;
     run mem (pc+4)
  | 2 ->
     bin ( * ) mem x y o;
     run mem (pc+4)
  | 99 -> ()
  | op -> raise (Invalid_opcode op)

let read_input () =
  let str = ref "" in
  (try
     while true;
     do str := !str ^ read_line ()
     done
  with End_of_file -> ());
  !str

let next_combination n v =
  if v + 1 = 100
  then (n + 1, 0)
  else (n, v + 1)
  
let rec try_combination c mem (n, v) =
  let clone = Hashtbl.copy mem in
  put clone 1 n;
  put clone 2 v;
  run clone 0;
  if (get clone 0 == c)
  then (n, v)
  else try_combination c mem (next_combination n v)
  
let () =
  let mem = Hashtbl.create 16 in
  let ints = String.split_on_char ',' (read_input ()) in
  let data = List.map int_of_string ints in
  List.iteri (put mem) data;
  let (n, v) = try_combination 19690720 mem (0, 0) in
  Format.printf "%d\n" (100*n + v)
