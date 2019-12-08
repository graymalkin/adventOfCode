open Format

type movement =
  Up of int
| Down of int
| Left of int
| Right of int

module CoordSet =
  Set.Make (struct
             type t = int * int * int
             let compare (x1, y1, _) (x2, y2, _) = Pervasives.compare (x1, y1) (x2, y2)
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

let coords (x, y, s) = function
    Up d -> List.map (fun yd -> x, y + yd, s+yd) (range 1 d)
  | Down d -> List.map (fun yd -> x, y - yd, s+yd) (range 1 d)
  | Left d -> List.map (fun xd -> x - xd, y, s+xd) (range 1 d)
  | Right d -> List.map (fun xd -> x + xd, y, s+xd) (range 1 d)

let edge (x, y, s) = function
    Up d -> x, y + d, s + d
  | Down d -> x, y - d, s + d
  | Left d -> x - d, y, s + d
  | Right d -> x + d, y, s + d
            
let rec wire_coords acc pos = function
    w :: ws ->
    let acc' =
      List.fold_right
        (fun c acc -> CoordSet.add c acc)
        (coords pos w) acc
    in
    wire_coords acc' (edge pos w) ws
  | [] -> acc
            
let manhatten_distance (x, y, _) = abs x + abs y

let cmp_steps (_, _, s1) (_, _, s2) = compare s1 s2
                                 
let () =
  let wire_a = parse (read_line ()) in

  let wire_b = parse (read_line ()) in

  let wire_coords_a = wire_coords CoordSet.empty (0,0,0) wire_a in
  let wire_coords_b = wire_coords CoordSet.empty (0,0,0) wire_b in

  let intersects = CoordSet.inter wire_coords_a wire_coords_b in
  
  let x, y, s = List.nth (List.sort cmp_steps (CoordSet.elements intersects)) 0 in

  let _, _, s1 = CoordSet.find (x, y, 0) wire_coords_a in
  let _, _, s2 = CoordSet.find (x, y, 0) wire_coords_b in
  
  printf "%d\n" (s1+s2)
  
