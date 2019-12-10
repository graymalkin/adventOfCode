open Format

exception Invalid_opcode of int
exception Invalid_mode of int
exception Invalid_address of int
exception End_of_list
                        
type mode = Immediate | Position | Relative

let get_relative_base mem = Hashtbl.find mem (-1)
let set_relative_base mode mem addr = 
   let current = get_relative_base mem in
   match mode with
   | Immediate -> 
      printf "immediate base offset: old: %d, new: %d\n" current (current + Hashtbl.find mem addr);
      Hashtbl.replace mem (-1) (current + Hashtbl.find mem addr)
   | Position -> 
      let a = try Hashtbl.find mem (Hashtbl.find mem addr) with Not_found -> 0 in
      printf "position base offset: old: %d, new: %d\n" current (current + a);
      Hashtbl.replace mem (-1) (current + a) 
   | Relative -> 
      let d = try Hashtbl.find mem (current + (Hashtbl.find mem addr)) with Not_found -> 0 in
      printf "relative base offset: old: %d, new: %d\n" current (current + d);
      Hashtbl.replace mem (-1) (current + d)

let get mode mem addr =
  try
    match mode with
    | Immediate -> Hashtbl.find mem addr
    | Position -> Hashtbl.find mem (Hashtbl.find mem addr)
    | Relative -> Hashtbl.find mem ((get_relative_base mem) + (Hashtbl.find mem addr))
  with Not_found -> 0
               
let put mode mem addr data =
   match mode with
   | Immediate -> Hashtbl.add mem addr data
   | Position -> Hashtbl.add mem (Hashtbl.find mem addr) data
   | Relative -> 
      let a = (get_relative_base mem) + (Hashtbl.find mem addr) in
      printf "mem[%d] = %d\n" a data;
      Hashtbl.add mem a data

let pop xs =
  match !xs with
    [] -> raise End_of_list
  | x :: xs' ->
     xs := xs'; x
     
let push xs x =
  xs := x :: !xs

let input () = read_int ()
let output x = printf ": %d\n" x
          
let mode = function 0 -> Position | 1 -> Immediate | 2 -> Relative | m -> raise (Invalid_mode m)
let mode_a op = mode @@ (op / 10000) mod 10
let mode_b op = mode @@ (op / 1000) mod 10
let mode_c op = mode @@ (op / 100) mod 10
let raw_op op = op mod 100

let rec run mem pc =
  let op = get Immediate mem pc in
  match raw_op op with
  | 1 -> (* ADD *)
     let x = get (mode_c op) mem (pc+1) in
     let y = get (mode_b op) mem (pc+2) in
     put (mode_c op) mem (pc+3) (x+y);
     run mem (pc+4)
  | 2 -> (* MUL *)
     let x = get (mode_c op) mem (pc+1) in
     let y = get (mode_b op) mem (pc+2) in
     put (mode_a op) mem (pc+3) (x*y);
     run mem (pc+4)
  | 3 -> (* INPUT *)
     put (mode_c op) mem (pc+1) (input ());
     run mem (pc+2)
  | 4 -> (* OUTPUT *)
     let x = get (mode_c op) mem (pc+1) in
     output x;
     run mem (pc+2)
  | 5 -> (* JUMP-IF-TRUE *)
     let x = get (mode_c op) mem (pc+1) in
     let pc' = get (mode_b op) mem (pc+2) in
     if x <> 0 then run mem pc'
     else run mem (pc+3)
  | 6 -> (* JUMP-IF-FALSE *)
     let x = get (mode_c op) mem (pc+1) in
     let pc' = get (mode_b op) mem (pc+2) in
     if x = 0 then run mem pc'
     else run mem (pc+3)
  | 7 -> (* LT *)
     let x = get (mode_c op) mem (pc+1) in
     let y = get (mode_b op) mem (pc+2) in
     if x < y
     then (put (mode_a op) mem (pc+3) 1; run mem (pc+4))
     else (put (mode_a op) mem (pc+3) 0; run mem (pc+4))
  | 8 -> (* EQ *)
     let x = get (mode_c op) mem (pc+1) in
     let y = get (mode_b op) mem (pc+2) in
     if x = y
     then (put (mode_a op) mem (pc+3) 1; run mem (pc+4))
     else (put (mode_a op) mem (pc+3) 0; run mem (pc+4))
  | 9 -> (* SET BASE OFFSET *)
     printf "[%05d@%d] " op pc;
     set_relative_base (mode_c op) mem (pc+1);
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

let program = "104,1125899906842624,99"
let program = "1102,34915192,34915192,7,4,7,99,0"
let program = "109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99"
let program = "1102,34463338,34463338,63,1007,63,34463338,63,1005,63,53,1101,3,0,1000,109,988,209,12,9,1000,209,6,209,3,203,0,1008,1000,1,63,1005,63,65,1008,1000,2,63,1005,63,904,1008,1000,0,63,1005,63,58,4,25,104,0,99,4,0,104,0,99,4,17,104,0,99,0,0,1101,25,0,1016,1102,760,1,1023,1102,1,20,1003,1102,1,22,1015,1102,1,34,1000,1101,0,32,1006,1101,21,0,1017,1102,39,1,1010,1101,30,0,1005,1101,0,1,1021,1101,0,0,1020,1102,1,35,1007,1102,1,23,1014,1102,1,29,1019,1101,767,0,1022,1102,216,1,1025,1102,38,1,1011,1101,778,0,1029,1102,1,31,1009,1101,0,28,1004,1101,33,0,1008,1102,1,444,1027,1102,221,1,1024,1102,1,451,1026,1101,787,0,1028,1101,27,0,1018,1101,0,24,1013,1102,26,1,1012,1101,0,36,1002,1102,37,1,1001,109,28,21101,40,0,-9,1008,1019,41,63,1005,63,205,1001,64,1,64,1105,1,207,4,187,1002,64,2,64,109,-9,2105,1,5,4,213,1106,0,225,1001,64,1,64,1002,64,2,64,109,-9,1206,10,243,4,231,1001,64,1,64,1105,1,243,1002,64,2,64,109,-3,1208,2,31,63,1005,63,261,4,249,1106,0,265,1001,64,1,64,1002,64,2,64,109,5,21108,41,41,0,1005,1012,287,4,271,1001,64,1,64,1105,1,287,1002,64,2,64,109,6,21102,42,1,-5,1008,1013,45,63,1005,63,307,1105,1,313,4,293,1001,64,1,64,1002,64,2,64,109,-9,1201,0,0,63,1008,63,29,63,1005,63,333,1106,0,339,4,319,1001,64,1,64,1002,64,2,64,109,-13,2102,1,4,63,1008,63,34,63,1005,63,361,4,345,1105,1,365,1001,64,1,64,1002,64,2,64,109,5,1201,7,0,63,1008,63,33,63,1005,63,387,4,371,1105,1,391,1001,64,1,64,1002,64,2,64,109,7,1202,1,1,63,1008,63,32,63,1005,63,411,1105,1,417,4,397,1001,64,1,64,1002,64,2,64,109,20,1205,-7,431,4,423,1106,0,435,1001,64,1,64,1002,64,2,64,109,2,2106,0,-3,1001,64,1,64,1105,1,453,4,441,1002,64,2,64,109,-7,21101,43,0,-9,1008,1014,43,63,1005,63,479,4,459,1001,64,1,64,1105,1,479,1002,64,2,64,109,-5,21108,44,43,0,1005,1018,495,1105,1,501,4,485,1001,64,1,64,1002,64,2,64,109,-7,1205,9,517,1001,64,1,64,1105,1,519,4,507,1002,64,2,64,109,11,1206,-1,531,1106,0,537,4,525,1001,64,1,64,1002,64,2,64,109,-15,1208,0,36,63,1005,63,557,1001,64,1,64,1106,0,559,4,543,1002,64,2,64,109,7,2101,0,-7,63,1008,63,35,63,1005,63,581,4,565,1106,0,585,1001,64,1,64,1002,64,2,64,109,-3,21107,45,46,4,1005,1015,607,4,591,1001,64,1,64,1105,1,607,1002,64,2,64,109,-16,2102,1,10,63,1008,63,31,63,1005,63,631,1001,64,1,64,1106,0,633,4,613,1002,64,2,64,109,1,2107,33,10,63,1005,63,649,1106,0,655,4,639,1001,64,1,64,1002,64,2,64,109,17,2101,0,-9,63,1008,63,31,63,1005,63,679,1001,64,1,64,1106,0,681,4,661,1002,64,2,64,109,-6,2107,34,0,63,1005,63,703,4,687,1001,64,1,64,1106,0,703,1002,64,2,64,109,5,1207,-5,34,63,1005,63,719,1105,1,725,4,709,1001,64,1,64,1002,64,2,64,109,-15,1202,6,1,63,1008,63,20,63,1005,63,751,4,731,1001,64,1,64,1105,1,751,1002,64,2,64,109,21,2105,1,5,1001,64,1,64,1106,0,769,4,757,1002,64,2,64,109,5,2106,0,5,4,775,1001,64,1,64,1106,0,787,1002,64,2,64,109,-27,1207,4,35,63,1005,63,809,4,793,1001,64,1,64,1106,0,809,1002,64,2,64,109,13,2108,33,-1,63,1005,63,831,4,815,1001,64,1,64,1106,0,831,1002,64,2,64,109,4,21107,46,45,1,1005,1014,851,1001,64,1,64,1105,1,853,4,837,1002,64,2,64,109,3,21102,47,1,-3,1008,1013,47,63,1005,63,875,4,859,1106,0,879,1001,64,1,64,1002,64,2,64,109,-9,2108,28,2,63,1005,63,895,1106,0,901,4,885,1001,64,1,64,4,64,99,21101,27,0,1,21102,1,915,0,1106,0,922,21201,1,59074,1,204,1,99,109,3,1207,-2,3,63,1005,63,964,21201,-2,-1,1,21102,942,1,0,1105,1,922,21201,1,0,-1,21201,-2,-3,1,21102,1,957,0,1105,1,922,22201,1,-1,-2,1106,0,968,22102,1,-2,-2,109,-3,2105,1,0"
  
let quine = "109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99"

let () =
   let mem = Hashtbl.create 16 in
   let ints = String.split_on_char ',' (program) in
   let data = List.map int_of_string ints in
   Hashtbl.add mem (-1) 0;
   List.iteri (put Immediate mem) data;
   run mem 0