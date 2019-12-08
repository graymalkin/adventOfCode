exception Invalid_opcode of int
exception Invalid_mode of int

type mode = Immediate | Position

type op = Mul | Add | In | Out | Jit | Jif | Lt | Eq | Stop

type operation =
  Multiply of mode * mode * mode
| Addition of mode * mode * mode
| JumpIfTrue of mode * mode
| JumpIfFalse of mode * mode
| LessThan of mode * mode * mode
| Equal of mode * mode * mode
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
  put mem addr (read_int ())

let output mem addr = 
  Format.printf " :: %d\n" (get Immediate mem addr)

let bin mo f mem x y o =
  put mem o (f x y)

let parse_opcode x =
  [(x/10000) mod 10; (x/1000) mod 10; (x/100) mod 10; x mod 100]

let lookup_basic_op = function
  1 -> Add
| 2 -> Mul
| 3 -> In
| 4 -> Out
| 5 -> Jit
| 6 -> Jif
| 7 -> Lt
| 8 -> Eq
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
    | In -> Input | Out -> Output 
    | Jit -> JumpIfTrue (lookup_mode mx, lookup_mode my)
    | Jif -> JumpIfFalse (lookup_mode mx, lookup_mode my)
    | Lt -> LessThan (lookup_mode mx, lookup_mode my, lookup_mode mo)
    | Eq -> Equal (lookup_mode mx, lookup_mode my, lookup_mode mo)
    | Stop -> Halt
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
  | JumpIfFalse (mx, mo) ->
    let x = get mx mem (pc+1) in
    let addr = get mo mem (pc+2) in
    if x == 0 
    then run mem addr
    else run mem (pc+3)
  | JumpIfTrue (mx, mo) ->
    let x = get mx mem (pc+1) in
    let addr = get mo mem (pc+2) in
    if x <> 0 
    then run mem addr
    else run mem (pc+3)
  | LessThan (mx, my, _) ->
    let x = get mx mem (pc+1) in
    let y = get my mem (pc+2) in
    let o = get Immediate mem (pc+3) in
    if x < y 
    then put mem o 1
    else put mem o 0;
    run mem (pc+4)
  | Equal (mx, my, _) ->
    let x = get mx mem (pc+1) in
    let y = get my mem (pc+2) in
    let o = get Immediate mem (pc+3) in
    if x == y 
    then put mem o 1
    else put mem o 0;
    run mem (pc+4)
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
  run mem 0