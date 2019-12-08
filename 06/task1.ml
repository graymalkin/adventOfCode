open Format
open Graph
module SolarGraph = Imperative.Digraph.Concrete(struct
  type t = string
  let compare = String.compare
  let hash = Hashtbl.hash
  let equal = (=)
end)
module SolarGraphBuilder = Builder.I(SolarGraph)
module SolarGraphOps = Oper.Make(SolarGraphBuilder)

let add_edge g s =
  let toks = String.split_on_char ')' s in
  if List.length toks = 2
  then SolarGraph.add_edge g (List.nth toks 0) (List.nth toks 1)

let parse_input () =
  let input = ref "" in
  (   try
      while true;
      do
        input := !input ^ read_line () ^ "\n"
      done
    with End_of_file -> ()
  );
  let g = SolarGraph.create () in
  ignore @@ List.map (add_edge g) (String.split_on_char '\n' !input);
  g

  
(** Tail recursive sum *)
let sum xs =
  let rec go acc = function
      [] -> acc
    | x :: xs ->  go (acc + x) xs
  in
  go 0 xs

let count_orbits g =
  let tg = SolarGraphOps.transitive_closure ~reflexive:false g in
  SolarGraph.fold_edges (fun _ _ a -> a + 1) tg 0
  
let () =
  let g = parse_input () in
  printf "%d\n" (count_orbits g)
