type position = int * int * int
type velocity = int * int * int
type body = position * velocity 

let component_gravity x y vx vy = 
  if x < y then (vx+1, vy - 1)
  else if x > y then (vx-1, vy+1)
  else (vx, vy)

let gravity ((px1, py1, pz1), (vx1, vy1, vz1)) ((px2, py2, pz2), (vx2, vy2, vz2)) = 
  let vx1, vx2 = component_gravity px1 px2 vx1 vx2 in
  let vy1, vy2 = component_gravity py1 py2 vy1 vy2 in
  let vz1, vz2 = component_gravity pz1 pz2 vz1 vz2 in
  ((px1, py1, pz1), (vx1, vy1, vz1)), ((px2, py2, pz2), (vx2, vy2, vz2))

let velocity ((px1, py1, pz1), (vx1, vy1, vz1)) = 
  let px1 = px1 + vx1 in
  let py1 = py1 + vy1 in
  let pz1 = pz1 + vz1 in
  ((px1, py1, pz1), (vx1, vy1, vz1))

let te_moon ((px, py, pz), (vx, vy, vz)) =
  let pe = abs px + abs py + abs pz in
  let ke = abs vx + abs vy + abs vz in
  pe * ke

let sum xs =
  let rec go acc = function
      [] -> acc
    | x :: xs ->  go (acc + x) xs
  in
  go 0 xs

let moons = [(-1,0,2); (2,-10,-7); (4,-8,8); (3,5,-1)]
let moons = [(14,15,-2); (17,-3,4); (6,12,-13); (-2,10,-8)]
let show_moon ((px, py, pz), (vx, vy, vz)) =
  Format.printf "pos=< x=%d, y=%d, z=%d>, vel=< x=%d, y=%d, z=%d>\n" px py pz vx vy vz

let step moons = 
  let moons = List.map (fun m ->
    let other_moons = List.filter ((<>) m) moons in
    List.fold_left (fun acc m' ->
      let acc, m' = gravity acc m' in
      acc
    ) m (other_moons) 
  ) moons
  in
  List.map velocity moons

let () =
  let io, europa, ganymede, callisto = (14,15,-2), (17,-3,4), (6,12,-13), (-2,10,-8) in
  let moons = ref (List.map (fun m -> (m, (0,0,0))) [io; europa; ganymede; callisto]) in
  let start_te = sum @@ List.map te_moon !moons in
  let start = !moons in
  let steps = ref 0 in 
  let complete = ref false in
  
  while !steps < 1000000000;
  do
    moons := step !moons;
    incr steps;
    if start_te = sum @@ (List.map te_moon !moons) then complete := true;
    (* if !moons = start then complete := true; *)
    if !steps mod 10000 = 0 then Format.printf "%d\n" !steps;
    flush_all ()
  done;
  let energy = List.map te_moon !moons in
  Format.printf "total energy: %d\n" (sum energy);
  Format.printf "steps for cycle = %d\n" !steps