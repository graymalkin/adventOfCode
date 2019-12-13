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
  with Not_found -> 0

let put mode mem addr data =
   match mode with
   | Immediate -> Hashtbl.add mem addr data
   | Position -> Hashtbl.add mem (Hashtbl.find mem addr) data

let input () = read_int ()
let output x = printf ": %d\n" x

let mode = function 0 -> Position | 1 -> Immediate | m -> raise (Invalid_mode m)
let mode_a op = mode @@ (op / 10000) mod 10
let mode_b op = mode @@ (op / 1000) mod 10
let mode_c op = mode @@ (op / 100) mod 10
let raw_op op = op mod 100

let rec run input output mem =
  let pc = Hashtbl.find mem (-1) in
  printf "pc: %d\n" pc;
  let op = Hashtbl.find mem pc in
  match raw_op op with
  | 1 -> (* ADD *)
     let x = get (mode_c op) mem (pc+1) in
     let y = get (mode_b op) mem (pc+2) in
     put (mode_a op) mem (pc+3) (x+y);
     Hashtbl.replace mem (-1) (pc+4); run input output mem
  | 2 -> (* MUL *)
     let x = get (mode_c op) mem (pc+1) in
     let y = get (mode_b op) mem (pc+2) in
     put (mode_a op) mem (pc+3) (x*y);
     Hashtbl.replace mem (-1) (pc+4); run input output mem
  | 3 -> (* INPUT *)
     put (mode_c op) mem (pc+1) (input ());
     Hashtbl.replace mem (-1) (pc+2); run input output mem
  | 4 -> (* OUTPUT *)
     let x = get (mode_c op) mem (pc+1) in
     output x;
     Hashtbl.replace mem (-1) (pc+2); run input output mem
  | 5 -> (* JUMP-IF-TRUE *)
     let x = get (mode_c op) mem (pc+1) in
     let pc' = get (mode_b op) mem (pc+2) in
     if x <> 0 then (Hashtbl.replace mem (-1) pc'; run input output mem)
     else (Hashtbl.replace mem (-1) (pc+3); run input output mem)
  | 6 -> (* JUMP-IF-FALSE *)
     let x = get (mode_c op) mem (pc+1) in
     let pc' = get (mode_b op) mem (pc+2) in
     if x = 0 then (Hashtbl.replace mem (-1) pc'; run input output mem)
     else (Hashtbl.replace mem (-1) (pc+3); run input output mem)
  | 7 -> (* LT *)
     let x = get (mode_c op) mem (pc+1) in
     let y = get (mode_b op) mem (pc+2) in
     if x < y
     then (put (mode_a op) mem (pc+3) 1; Hashtbl.replace mem (-1) (pc+4); run input output mem)
     else (put (mode_a op) mem (pc+3) 0; Hashtbl.replace mem (-1) (pc+4); run input output mem)
  | 8 -> (* EQ *)
     let x = get (mode_c op) mem (pc+1) in
     let y = get (mode_b op) mem (pc+2) in
     if x = y
     then (put (mode_a op) mem (pc+3) 1; Hashtbl.replace mem (-1) (pc+4); run input output mem)
     else (put (mode_a op) mem (pc+3) 0; Hashtbl.replace mem (-1) (pc+4); run input output mem)
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

exception Blocked
let pop xs = 
   match !xs with
    [] -> raise Blocked
   | x :: rest -> xs := rest; x

let program p =
   let mem = Hashtbl.create 16 in
   let ints = String.split_on_char ',' "3,8,1001,8,10,8,105,1,0,0,21,38,47,72,97,122,203,284,365,446,99999,3,9,1001,9,3,9,1002,9,5,9,1001,9,4,9,4,9,99,3,9,102,3,9,9,4,9,99,3,9,1001,9,2,9,102,5,9,9,101,3,9,9,1002,9,5,9,101,4,9,9,4,9,99,3,9,101,5,9,9,1002,9,3,9,101,2,9,9,102,3,9,9,1001,9,2,9,4,9,99,3,9,101,3,9,9,102,2,9,9,1001,9,4,9,1002,9,2,9,101,2,9,9,4,9,99,3,9,1001,9,2,9,4,9,3,9,101,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,102,2,9,9,4,9,3,9,101,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,101,2,9,9,4,9,3,9,101,1,9,9,4,9,3,9,1001,9,2,9,4,9,99,3,9,1001,9,1,9,4,9,3,9,101,1,9,9,4,9,3,9,101,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,101,2,9,9,4,9,3,9,101,1,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,1,9,9,4,9,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,99,3,9,1001,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,101,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,101,1,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,102,2,9,9,4,9,99,3,9,101,1,9,9,4,9,3,9,101,1,9,9,4,9,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,101,2,9,9,4,9,3,9,101,2,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,1,9,4,9,99,3,9,101,2,9,9,4,9,3,9,101,2,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,101,1,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,2,9,9,4,9,3,9,1001,9,2,9,4,9,99" in
   (* let ints = String.split_on_char ',' "3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5" in *)
   let data = List.map int_of_string ints in
   List.iteri (put Immediate mem) data;
   Hashtbl.add mem (-1) 0;
   mem

let run_perm perm =
   let pa , pb , pc , pd , pe  = match perm with
      pa :: pb :: pc :: pd :: pe :: [] -> pa , pb , pc , pd , pe
    | _ -> raise (Invalid_argument "bad phase setting")
   in
   let ayyz = ref [pa;0] in
   let bs = ref [pb] in
   let cs = ref [pc] in
   let ds = ref [pd] in
   let es = ref [pe] in

   let mem = program program in
   let mem_as = Hashtbl.copy mem in
   let mem_bs = Hashtbl.copy mem in
   let mem_cs = Hashtbl.copy mem in
   let mem_ds = Hashtbl.copy mem in
   let mem_es = Hashtbl.copy mem in

   let input_as () = pop ayyz in
   let input_bs () = pop bs in
   let input_cs () = pop cs in
   let input_ds () = pop ds in
   let input_es () = pop es in

   let output_as x = bs := !bs @ [x] in
   let output_bs x = cs := !cs @ [x] in
   let output_cs x = ds := !ds @ [x] in
   let output_ds x = es := !es @ [x] in
   let output_es x = ayyz := !ayyz @ [x] in

   let complete = ref [] in
   while List.sort compare !complete <> [0;1;2;3;4] ;
   do
      (
         try 
            if not (List.mem 0 !complete)
            then (printf "A"; run input_as output_as mem_as; printf "A complete\n"; complete := 0 :: !complete)
         with Blocked -> ()
      );
      (
         try 
            if not (List.mem 1 !complete)
            then (printf "B"; run input_bs output_bs mem_bs; printf "B complete\n"; complete := 1 :: !complete)
         with Blocked -> ()
      );
      (
         try 
            if not (List.mem 2 !complete)
            then (printf "C"; run input_cs output_cs mem_cs; printf "C complete\n"; complete := 2 :: !complete)
         with Blocked -> ()
      );
      (
         try 
            if not (List.mem 3 !complete)
            then (printf "D"; run input_ds output_ds mem_ds; printf "D complete\n"; complete := 3 :: !complete)
         with Blocked -> ()
      );
      (
         try 
            if not (List.mem 4 !complete)
            then (printf "E"; run input_es output_es mem_es; printf "E complete\n"; complete := 4 :: !complete)
         with Blocked -> ()
      );
   done;
   match !ayyz with 
    x :: [] -> x
   | _ -> raise Not_found

let () =
   let perms = permutations [5;6;7;8;9] in
   let outputs = List.map (run_perm) perms in
   List.iter (printf "%d\n") (List.sort compare outputs)