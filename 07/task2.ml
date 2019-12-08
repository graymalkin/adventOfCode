open Format

exception Invalid_opcode of int
exception Invalid_mode of int
exception Invalid_address of int
exception End_of_list
                        
type mode = Immediate | Position
                          
let get mode mem addr =
  try
    match mode with
    | Immediate -> Hashtbl.find mem addr
    | Position -> Hashtbl.find mem (Hashtbl.find mem addr)
  with Not_found -> raise (Invalid_address addr)
               
let put mem addr data = Hashtbl.add mem addr data

let mode = function 0 -> Position | 1 -> Immediate | m -> raise (Invalid_mode m)
let mode_a op = mode @@ (op / 10000) mod 10
let mode_b op = mode @@ (op / 1000) mod 10
let mode_c op = mode @@ (op / 100) mod 10
let raw_op op = op mod 100

let run input output mem pc = 
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
    | 99 -> ()
    | op ->
       printf "pc: %d \n" pc;
       raise (Invalid_opcode op)
  in
  run mem pc

let read_input () =
  let str = ref "" in
  (try
     while true;
     do str := !str ^ read_line ()
     done
  with End_of_file -> ());
  !str

let program = "3,8,1001,8,10,8,105,1,0,0,21,38,47,72,97,122,203,284,365,446,99999,3,9,1001,9,3,9,1002,9,5,9,1001,9,4,9,4,9,99,3,9,102,3,9,9,4,9,99,3,9,1001,9,2,9,102,5,9,9,101,3,9,9,1002,9,5,9,101,4,9,9,4,9,99,3,9,101,5,9,9,1002,9,3,9,101,2,9,9,102,3,9,9,1001,9,2,9,4,9,99,3,9,101,3,9,9,102,2,9,9,1001,9,4,9,1002,9,2,9,101,2,9,9,4,9,99,3,9,1001,9,2,9,4,9,3,9,101,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,102,2,9,9,4,9,3,9,101,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,101,2,9,9,4,9,3,9,101,1,9,9,4,9,3,9,1001,9,2,9,4,9,99,3,9,1001,9,1,9,4,9,3,9,101,1,9,9,4,9,3,9,101,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,101,2,9,9,4,9,3,9,101,1,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,1,9,9,4,9,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,99,3,9,1001,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,101,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,101,1,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,102,2,9,9,4,9,99,3,9,101,1,9,9,4,9,3,9,101,1,9,9,4,9,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,101,2,9,9,4,9,3,9,101,2,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,1,9,4,9,99,3,9,101,2,9,9,4,9,3,9,101,2,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,101,1,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,2,9,9,4,9,3,9,1001,9,2,9,4,9,99"

let program = "3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5"

  
let run_combination mem c = List.fold_left (amp (Hashtbl.copy mem)) 0 c

let ins_all_positions x l =  
  let rec aux prev acc = function
    | [] -> (prev @ [x]) :: acc |> List.rev
    | hd::tl as l -> aux (prev @ [hd]) ((prev @ [x] @ l) :: acc) tl
  in
  aux [] [] l
              
let rec permutations = function  
  | [] -> []
  | x::[] -> [[x]] (* we must specify this edge case *)
  | x::xs -> List.fold_left (fun acc p -> acc @ ins_all_positions x p ) [] (permutations xs)

                          
let () =
  let mem = Hashtbl.create 16 in
  let ints = String.split_on_char ',' program in
  let data = List.map int_of_string ints in
  List.iteri (put mem) data;
  let perms = permutations [5;6;7;8;9] in
  let vals = List.map (run_combination mem) perms in
  
  printf "max value: %d\n" (List.nth (List.rev @@ List.sort compare vals) 0)
