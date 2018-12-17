type vertex = string
type adjacency = vertex * vertex list
type graph = adjacency list

let string_of_vertex_list lst =
  let rec aux l =
    match l with
    | [] -> " ]"
    | hd :: tl -> hd ^ aux tl
  in
  "[ " ^ aux lst

let string_of_graph g =
  let string_of_adjacency (s, lst) =
    "(" ^ s ^ ", " ^ string_of_vertex_list lst ^ ")\n"
  in
  let rec aux lst =
    match lst with
    | [] -> ""
    | hd :: tl -> string_of_adjacency hd ^ aux tl
  in
  aux g

let generate_tuple line =
  Scanf.sscanf line "Step %s must be finished before step %s can begin." (fun r c -> (r, c))

let insert_in_graph graph (req, conc) : graph =
  let matches, doesnt_match = List.partition (fun (a, b) -> a = req) graph in
  match matches with
  | []                      -> (req, [conc]) :: doesnt_match
  | (_, conclusions) :: tl  -> (req, conc :: conclusions) :: doesnt_match

let generate_graph ic : graph =
  let rec aux g =
    try
      begin
        input_line ic
        |> generate_tuple
        |> insert_in_graph g
        |> aux
      end
    with End_of_file -> g
  in aux []

let vertices graph =
  let rec aux g lst =
    match g with
    | [] -> lst
    | (v, _) :: tl -> aux tl (v :: lst)
  in
  aux graph []

let unrestricted_vertices graph =
  let rec vertices_in_conclusions g lst =
    match g with
    | [] -> lst
    | (_, c) :: tl -> vertices_in_conclusions tl (c @ lst)
  in
  let v = vertices_in_conclusions graph [] in
  vertices graph
  |> List.filter (fun elt -> not (List.mem elt v))
  |> List.sort compare

let remove_adjacency graph restriction =
  List.filter (fun (r, _) -> r != restriction) graph

let main () =
  let ic = open_in "input.txt" in
  (* let ic = open_in "example.txt" in *)
  let rec aux (g : graph) (lst : vertex list) =
    (* print_endline (string_of_graph g) ; *)
    match unrestricted_vertices g with
    | hd :: tl when List.length g > 1 -> aux (remove_adjacency g hd) (lst @ [hd])
    | hd :: _ -> let (_, l) = List.hd g in lst @ [hd] @ [List.hd l]
    | [] -> lst
  in
  aux (generate_graph ic) []
  |> string_of_vertex_list
  |> print_endline
  ;
  close_in ic

let () =
  main()
