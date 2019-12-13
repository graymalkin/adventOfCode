open Format

let rec pop_n xs n =
    match n with
      0 -> [], xs
    | n -> 
        let rest, xs = pop_n xs (n - 1) in
        match xs with 
            [] -> raise Not_found
        | x :: xs -> rest @ [x], xs

let rec pop_ns xs n =
    let xs, rest = pop_n xs n in
    try
        xs :: pop_ns rest n
    with Not_found -> []

let get_input () =
    let input_string = read_line () in
    let data = ref [] in
    String.iter (fun c ->
        let d = int_of_char c - int_of_char '0' in
        data := d :: !data
    ) input_string;
    !data

let rec count a = function
    x :: xs -> 
    if a = x 
    then 1 + count a xs 
    else count a xs
| [] -> 0

let sum xs =
  let rec go acc = function
      [] -> acc
    | x :: xs ->  go (acc + x) xs
  in
  go 0 xs

let pop = function
  x :: xs -> xs
| [] -> []

let rec n_zip f = function
    [] -> []
 | xss ->
    try
        let xs = List.map (fun z -> List.nth z 0) xss in
        f xs :: (n_zip f (List.map pop xss))
    with Failure _ -> []

type color = Black | White 

let rec blend = function
  0 :: _ -> Black
| 1 :: _ -> White
| 2 :: xs -> blend xs (* transparent *)
| _ -> White

let show_pixel = function Black -> " " | White -> "#"

let () =
    let is = get_input () in
    let layers = pop_ns is (25*6) in
    let image = n_zip blend layers in
    let ps = List.rev @@ pop_ns image 25 in
    List.iter (fun line -> List.iter (fun p -> printf "%s" (show_pixel p)) line; printf "\n") ps