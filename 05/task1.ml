exception Invalid_opcode of int
exception Invalid_mode of int

type mode = Immediate | Position

type op = Mul | Add | In | Out | Stop

type operation =
  Multiply of mode * mode * mode
| Addition of mode * mode * mode
| Input
| Output
| Halt

let get mode mem addr =
  match mode with 
    Position -> 
    Hashtbl.find mem (Hashtbl.find mem addr)
  | Immediate -> 
    Hashtbl.find mem addr

let put mem addr data = 
  Hashtbl.add mem addr data

let input mem addr =
  let data = read_int () in
  put mem addr data

let output mem addr = 
  Format.printf "%d\n" (get Immediate mem addr)

let bin mo f mem x y o =
  put mem o (f x y)

let parse_opcode x =
  [(x/10000) mod 10; (x/1000) mod 10; (x/100) mod 10; x mod 100]

let lookup_basic_op = function
  1 -> Add
| 2 -> Mul
| 3 -> In
| 4 -> Out
| 99 -> Stop
| n -> raise (Invalid_opcode n)

let lookup_mode = function
   0 -> Position
 | 1 -> Immediate
 | n -> raise (Invalid_mode n)

let show_mode = function Position -> "pos" | Immediate -> "imm"

let decode = function
  [mo; my; mx; op] -> 
    let basic_op = lookup_basic_op op in
    (match basic_op with
      Add -> Addition (lookup_mode mx, lookup_mode my, lookup_mode mo)
    | Mul -> Multiply (lookup_mode mx, lookup_mode my, lookup_mode mo)
    | In -> Input | Out -> Output | Stop -> Halt
    )  
  | _ -> raise (Invalid_argument "cannot decode operation")

let rec run mem pc =
  let opcode = get Immediate mem pc in
  let operation = decode @@ parse_opcode opcode in
  match operation with
  | Addition (mx, my, mo) ->
     let x = get mx mem (pc+1) in
     let y = get my mem (pc+2) in
     let o = get Immediate mem (pc+3) in
     bin mo (+) mem x y o;
     run mem (pc+4)
  | Multiply (mx, my, mo) ->
     let x = get mx mem (pc+1) in
     let y = get my mem (pc+2) in
     let o = get Immediate mem (pc+3) in
     bin mo ( * ) mem x y o;
     run mem (pc+4)
  | Input ->
     let x = get Immediate mem (pc+1) in
     input mem x;
     run mem (pc+2)
  | Output ->
     let x = get Immediate mem (pc+1) in
     output mem x;
     run mem (pc+2)
  | Halt -> ()

let read_input () =
  let str = ref "" in
  (try
     while true;
     do str := !str ^ read_line ()
     done
  with End_of_file -> ());
  !str
  
let () =
  let mem = Hashtbl.create 16 in
  let ints = String.split_on_char ',' (read_line ()) in
  let data = List.map int_of_string ints in
  List.iteri (put mem) data;
  run mem 0;
  Format.printf "%d\n" (get Immediate mem 0)
