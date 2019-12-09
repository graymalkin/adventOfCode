open Format

exception Invalid_opcode of int
exception Invalid_mode of int
exception Invalid_address of int
exception End_of_list
                        
type mode = Immediate | Position | Relative

let get_relative_base mem = Hashtbl.find mem (-1)
let set_relative_base mem data = Hashtbl.add mem (-1) data
                                 
let get mode mem addr =
  try
    match mode with
    | Immediate -> Hashtbl.find mem addr
    | Position -> Hashtbl.find mem (Hashtbl.find mem addr)
    | Relative -> Hashtbl.find mem (get_relative_base mem + addr)
  with Not_found -> 0
               
let put mem addr data = Hashtbl.add mem addr data

let pop xs =
  match !xs with
    [] -> raise End_of_list
  | x :: xs' ->
     xs := xs'; x
     
let push xs x =
  xs := x :: !xs

let input () = read_int ()
let output x = printf ": %d\n" x
          
let mode = function 0 -> Position | 1 -> Immediate | 2 -> Position | m -> raise (Invalid_mode m)
let mode_a op = mode @@ (op / 10000) mod 10
let mode_b op = mode @@ (op / 1000) mod 10
let mode_c op = mode @@ (op / 100) mod 10
let raw_op op = op mod 100

let rec run mem pc =
  let operation = get Immediate mem pc in
  match raw_op operation with
  | 1 -> (* ADD *)
     let x = get (mode_c operation) mem (pc+1) in
     let y = get (mode_b operation) mem (pc+2) in
     let o = get Immediate mem (pc+3) in
     put mem o (x+y);
     run mem (pc+4)
  | 2 -> (* MUL *)
     let x = get (mode_c operation) mem (pc+1) in
     let y = get (mode_b operation) mem (pc+2) in
     let o = get Immediate mem (pc+3) in
     put mem o (x*y);
     run mem (pc+4)
  | 3 -> (* INPUT *)
     let x = get Immediate mem (pc+1) in
     put mem x (input ());
     run mem (pc+2)
  | 4 -> (* OUTPUT *)
     let x = get (mode_c operation) mem (pc+1) in
     output x;
     run mem (pc+2)
  | 5 -> (* JUMP-IF-TRUE *)
     let x = get (mode_c operation) mem (pc+1) in
     let pc' = get (mode_b operation) mem (pc+2) in
     if x <> 0 then run mem pc'
     else run mem (pc+3)
  | 6 -> (* JUMP-IF-FALSE *)
     let x = get (mode_c operation) mem (pc+1) in
     let pc' = get (mode_b operation) mem (pc+2) in
     if x = 0 then run mem pc'
     else run mem (pc+3)
  | 7 -> (* LT *)
     let x = get (mode_c operation) mem (pc+1) in
     let y = get (mode_b operation) mem (pc+2) in
     let o = get Immediate mem (pc+3) in
     if x < y
     then (put mem o 1; run mem (pc+4))
     else (put mem o 0; run mem (pc+4))
  | 8 -> (* EQ *)
     let x = get (mode_c operation) mem (pc+1) in
     let y = get (mode_b operation) mem (pc+2) in
     let o = get Immediate mem (pc+3) in
     if x = y
     then (put mem o 1; run mem (pc+4))
     else (put mem o 0; run mem (pc+4))
  | 9 -> (* SET BASE OFFSET *)
     let x = get (mode_c operation) mem (pc+1) in
     set_relative_base mem x;
     run mem (pc+2)
  | 99 -> ()
  | op ->
     printf "pc: %d \n" pc;
     raise (Invalid_opcode op)

let read_input () =
  let str = ref "" in
  (try
     while true;
     do str := !str ^ read_line ()
     done
  with End_of_file -> ());
  !str

let program = "109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99"
                          
let () =
  let mem = Hashtbl.create 16 in
  let ints = String.split_on_char ',' program in
  let data = List.map int_of_string ints in
  set_relative_base mem 0;
  List.iteri (put mem) data;
  run mem 0
