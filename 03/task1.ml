open Format

type movement =
  Up of int
| Down of int
| Left of int
| Right of int

module CoordSet =
  Set.Make (struct
             type t = int * int
             let compare (x1, y1) (x2, y2) =
               if x1 = x2 then y1 - y2
               else x1 - x2
           end)

let parse str =
  let toks = String.split_on_char ',' str in
  List.map (fun tok ->
      let fst = tok.[0] in
      let rest = match String.split_on_char fst tok with
          fst :: r :: []  -> r
        | _ -> raise (Invalid_argument tok)
      in
      let data = int_of_string rest in
      match fst with
        'U' -> Up data
      | 'D' -> Down data
      | 'L' -> Left data
      | 'R' -> Right data
      | _ -> raise (Invalid_argument tok)
    ) toks

let range a b =
  assert (b >= a);
  let rec go acc a b =
    if a = b then acc
    else go (a::acc) (a+1) b
  in
  go [] a b
  
let coords (x, y) = function
    Up d -> List.map (fun yd -> x, y + yd) (range 1 d)
  | Down d -> List.map (fun yd -> x, y - yd) (range 1 d)
  | Left d -> List.map (fun xd -> x - xd, y) (range 1 d)
  | Right d -> List.map (fun xd -> x + xd, y) (range 1 d)

let edge (x, y) = function
    Up d -> x, y + d
  | Down d -> x, y - d
  | Left d -> x - d, y
  | Right d -> x + d, y
            
let rec wire_coords acc pos = function
    w :: ws ->
    let acc' =
      List.fold_right
        (fun c acc -> CoordSet.add c acc)
        (coords pos w) acc
    in
    wire_coords acc' (edge pos w) ws
  | [] -> acc
            
let manhatten_distance (x, y) = abs x + abs y
                                 
let () =
  let wire_a = parse (read_line ()) in
  let wire_b = parse (read_line ()) in

  let wire_coords_a = wire_coords CoordSet.empty (0,0) wire_a in
  let wire_coords_b = wire_coords CoordSet.empty (0,0) wire_b in

  let intersects = CoordSet.elements @@ CoordSet.inter wire_coords_a wire_coords_b in
  let distances = List.map manhatten_distance intersects in
  let d = List.nth (List.sort (-) distances) 0 in

  printf "%d\n" d
