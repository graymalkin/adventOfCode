open Format

let next_digit n =
  if n = 9 then 0, true
  else n + 1, false

let next_number ns =
  let ns, _ =
    List.fold_left (fun (ns', carry) n ->
        if carry then
          let n', carry' = next_digit n in
        n' :: ns', carry'
        else n :: ns', carry
      ) ([], true) (List.rev ns) in
  ns

let max a b = if a > b then a else b
  
let check_increasing ns =
  let rec go prev = function
      n :: ns -> n >= prev && go (max n prev) ns
    | [] -> true
  in
  go 0 ns
  
let check_repeats ns =
  let rec go prev = function
      n :: ns -> n = prev || go n ns
    | [] -> false
  in
  go 10 ns
  
let () =
  let count = ref 0 in
  let num = ref [1;6;8;6;3;0] in
  while !num <> [7;1;8;0;9;8];
  do
    if check_increasing !num && check_repeats !num
    then incr count;

    num := next_number !num
  done;
  Format.printf "%d\n" !count
    
