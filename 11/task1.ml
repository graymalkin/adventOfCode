open Format

exception Invalid_opcode of int
exception Invalid_mode of int
                        
type mode = Immediate | Position | Relative

let base = ref 0

let get mode mem addr =
  try
    match mode with
    | Immediate -> Hashtbl.find mem addr
    | Position -> Hashtbl.find mem (Hashtbl.find mem addr)
    | Relative -> Hashtbl.find mem (!base + (Hashtbl.find mem addr))
  with Not_found -> 0

let set_relative_base mode mem addr = 
   base := (!base + get mode mem addr)

let put mode mem addr data =
   match mode with
   | Immediate -> Hashtbl.add mem addr data
   | Position -> Hashtbl.add mem (Hashtbl.find mem addr) data
   | Relative -> Hashtbl.add mem (!base + (Hashtbl.find mem addr)) data

let input () = read_int ()
let output x = printf ": %d\n" x

let mode = function 0 -> Position | 1 -> Immediate | 2 -> Relative | m -> raise (Invalid_mode m)
let mode_a op = mode @@ (op / 10000) mod 10
let mode_b op = mode @@ (op / 1000) mod 10
let mode_c op = mode @@ (op / 100) mod 10
let raw_op op = op mod 100

let rec run input output mem pc =
  let op = Hashtbl.find mem pc in
  match raw_op op with
  | 1 -> (* ADD *)
     let x = get (mode_c op) mem (pc+1) in
     let y = get (mode_b op) mem (pc+2) in
     put (mode_a op) mem (pc+3) (x+y);
     run input output mem (pc+4)
  | 2 -> (* MUL *)
     let x = get (mode_c op) mem (pc+1) in
     let y = get (mode_b op) mem (pc+2) in
     put (mode_a op) mem (pc+3) (x*y);
     run input output mem (pc+4)
  | 3 -> (* INPUT *)
     put (mode_c op) mem (pc+1) (input ());
     run input output mem (pc+2)
  | 4 -> (* OUTPUT *)
     let x = get (mode_c op) mem (pc+1) in
     output x;
     run input output mem (pc+2)
  | 5 -> (* JUMP-IF-TRUE *)
     let x = get (mode_c op) mem (pc+1) in
     let pc' = get (mode_b op) mem (pc+2) in
     if x <> 0 then run input output mem pc'
     else run input output mem (pc+3)
  | 6 -> (* JUMP-IF-FALSE *)
     let x = get (mode_c op) mem (pc+1) in
     let pc' = get (mode_b op) mem (pc+2) in
     if x = 0 then run input output mem pc'
     else run input output mem (pc+3)
  | 7 -> (* LT *)
     let x = get (mode_c op) mem (pc+1) in
     let y = get (mode_b op) mem (pc+2) in
     if x < y
     then (put (mode_a op) mem (pc+3) 1; run input output mem (pc+4))
     else (put (mode_a op) mem (pc+3) 0; run input output mem (pc+4))
  | 8 -> (* EQ *)
     let x = get (mode_c op) mem (pc+1) in
     let y = get (mode_b op) mem (pc+2) in
     if x = y
     then (put (mode_a op) mem (pc+3) 1; run input output mem (pc+4))
     else (put (mode_a op) mem (pc+3) 0; run input output mem (pc+4))
  | 9 -> (* SET BASE OFFSET *)
     set_relative_base (mode_c op) mem (pc+1);
     run input output mem (pc+2)
  | 99 -> ()
  | op ->
     printf "pc: %d \n" pc;
     raise (Invalid_opcode op)

let run_program input output p =
   let mem = Hashtbl.create 16 in
   let ints = String.split_on_char ',' p in
   let data = List.map int_of_string ints in
   List.iteri (put Immediate mem) data;
   run input output mem 0

type direction = Up | Right | Down | Left
type color = Black | White

let move (x,y) d = 
  match d with
   Up -> (x, y+1)
 | Right -> (x+1, y)
 | Down -> (x, y-1)
 | Left -> (x-1, y)

let rotate_left = function Up -> Left | Left -> Down | Down -> Right | Right -> Up
let rotate_right = function Up -> Right | Right -> Down | Down -> Left | Left -> Up
let rotate = function 
   0 -> rotate_left 
 | 1 -> rotate_right 
 | _ -> raise (Invalid_argument "Unsupported rotation")

let decode_robot_output = function
   paint :: direction :: [] -> (paint, direction)
 | _ -> raise (Invalid_argument "Invalid output stream")

let move_robot =
   let direction = ref Up in 
   fun picture position output ->
      let paint, rot = decode_robot_output output in
      let put = if Hashtbl.mem picture !position then Hashtbl.replace else Hashtbl.add in
      (match paint with
        0 -> put picture !position Black
      | 1 -> put picture !position White
      | _ -> raise (Invalid_argument "Invalid paint operation!"));
      direction := rotate rot !direction;
      position := move !position !direction

let robot () = 
   let p = "3,8,1005,8,310,1106,0,11,0,0,0,104,1,104,0,3,8,102,-1,8,10,1001,10,1,10,4,10,108,1,8,10,4,10,1002,8,1,28,1,105,11,10,3,8,102,-1,8,10,1001,10,1,10,4,10,1008,8,0,10,4,10,102,1,8,55,3,8,102,-1,8,10,1001,10,1,10,4,10,108,0,8,10,4,10,1001,8,0,76,3,8,1002,8,-1,10,101,1,10,10,4,10,108,0,8,10,4,10,102,1,8,98,1,1004,7,10,1006,0,60,3,8,102,-1,8,10,1001,10,1,10,4,10,108,0,8,10,4,10,1002,8,1,127,2,1102,4,10,1,1108,7,10,2,1102,4,10,2,101,18,10,3,8,1002,8,-1,10,1001,10,1,10,4,10,1008,8,0,10,4,10,102,1,8,166,1006,0,28,3,8,1002,8,-1,10,101,1,10,10,4,10,108,1,8,10,4,10,101,0,8,190,1006,0,91,1,1108,5,10,3,8,1002,8,-1,10,101,1,10,10,4,10,1008,8,1,10,4,10,1002,8,1,220,1,1009,14,10,2,1103,19,10,2,1102,9,10,2,1007,4,10,3,8,1002,8,-1,10,101,1,10,10,4,10,1008,8,1,10,4,10,101,0,8,258,2,3,0,10,1006,0,4,3,8,102,-1,8,10,1001,10,1,10,4,10,108,1,8,10,4,10,1001,8,0,286,1006,0,82,101,1,9,9,1007,9,1057,10,1005,10,15,99,109,632,104,0,104,1,21102,1,838479487636,1,21102,327,1,0,1106,0,431,21102,1,932813579156,1,21102,1,338,0,1106,0,431,3,10,104,0,104,1,3,10,104,0,104,0,3,10,104,0,104,1,3,10,104,0,104,1,3,10,104,0,104,0,3,10,104,0,104,1,21101,0,179318033447,1,21101,385,0,0,1105,1,431,21101,248037678275,0,1,21101,0,396,0,1105,1,431,3,10,104,0,104,0,3,10,104,0,104,0,21101,0,709496558348,1,21102,419,1,0,1105,1,431,21101,825544561408,0,1,21101,0,430,0,1106,0,431,99,109,2,22101,0,-1,1,21101,40,0,2,21102,462,1,3,21101,0,452,0,1106,0,495,109,-2,2105,1,0,0,1,0,0,1,109,2,3,10,204,-1,1001,457,458,473,4,0,1001,457,1,457,108,4,457,10,1006,10,489,1101,0,0,457,109,-2,2106,0,0,0,109,4,2101,0,-1,494,1207,-3,0,10,1006,10,512,21101,0,0,-3,22101,0,-3,1,22101,0,-2,2,21101,1,0,3,21102,531,1,0,1105,1,536,109,-4,2105,1,0,109,5,1207,-3,1,10,1006,10,559,2207,-4,-2,10,1006,10,559,22101,0,-4,-4,1106,0,627,21202,-4,1,1,21201,-3,-1,2,21202,-2,2,3,21102,578,1,0,1105,1,536,22101,0,1,-4,21101,1,0,-1,2207,-4,-2,10,1006,10,597,21102,0,1,-1,22202,-2,-1,-2,2107,0,-3,10,1006,10,619,21201,-1,0,1,21102,1,619,0,105,1,494,21202,-2,-1,-2,22201,-4,-2,-4,109,-5,2106,0,0" in

   let picture = Hashtbl.create 64 in 
   let position = ref (0, 0) in
   let output = ref [] in
   let outputf x = 
      output := !output @ [x];
      if (List.length !output) = 2
      then (move_robot picture position !output; output := [])
   in
   let inputf () =
      try (match Hashtbl.find picture !position with Black -> 0 | White -> 1) with Not_found -> 0
   in
   (* Hashtbl.add picture (0,0) White; *)
   run_program inputf outputf p;
   picture

let rec range start finish =
   if start = finish 
   then [finish]
   else start :: range (start + 1) finish

let draw_line picture y =
   List.iter (fun x ->
      try
         match Hashtbl.find picture (x,y) with 
            Black -> printf " "
          | White -> printf "#"
      with Not_found -> printf "."
   ) (range (-60) 50)

let draw_image picture = List.iter (fun y -> printf "%04d " y; draw_line picture y; printf "\n") (List.rev @@ range (-50) 50)

let () = 
   let picture = robot () in
   draw_image picture;
   printf "%d\n" (Hashtbl.length picture)
