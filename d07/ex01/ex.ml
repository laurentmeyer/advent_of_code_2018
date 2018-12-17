type vertex = string
type adjacency = vertex * vertex list
type graph = adjacency list
type countdown = (vertex * int) list

let string_of_countdown c =
  let string_of_elt (s, i) = "(" ^ s ^ "," ^ string_of_int i ^ ")" in
  let rec aux lst =
    match lst with
    | [] ->  " ]"
    | hd :: tl -> string_of_elt hd ^ aux tl
  in "[ " ^ aux c

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

let generate_countdown (graph : graph) =
  graph
  |> List.map (fun (s, l) -> s :: l)
  |> List.flatten
  |> List.sort_uniq compare
  |> List.map (fun t -> (t, int_of_char t.[0] - int_of_char 'A' + 61))

let countdown_sum countdown =
  List.map (fun (_, t) -> t) countdown
  |> List.fold_left (+) 0

let countdown_for_vertex countdown vertex =
  let (_, t) = List.find (fun (v, _) -> v = vertex) countdown in t

let get_first i lst = 
  let rec aux src dst c =
    match src with
    | [] -> dst
    | _ when c = 0 -> dst
    | hd :: tl -> aux tl (hd :: dst) (c - 1)
  in aux lst [] i

let vertices (graph : graph) =
  let rec aux g lst =
    match g with
    | [] -> lst
    | (v, _) :: tl -> aux tl (v :: lst)
  in
  aux graph []

let adjacency graph vertex =
  let (_, l) = List.find (fun (v, _) -> v = vertex) graph in l

let done_vertex (countdown : countdown) vertex =
  let (_, t) = List.find (fun (v, _) -> v = vertex) countdown in t = 0

let adjacency_done graph countdown vertex =
  adjacency graph vertex
  |> List.map (done_vertex countdown)
  |> List.for_all ((=) true)

let available_vertices graph countdown =
  vertices graph
  |> List.filter (fun v -> countdown_for_vertex countdown v > 0)
  |> List.filter (adjacency_done graph countdown)

let insert_in_graph graph (req, conc) : graph =
  let matches, doesnt_match = List.partition (fun (a, _) -> a = conc) graph in
  let req_in_graph = List.exists (fun (a, _) -> a = req) graph in
  let with_conc = match matches with
    | []                      -> (conc, [req]) :: doesnt_match
    | (_, requirements) :: tl  -> (conc, req :: requirements) :: doesnt_match
  in
  if req_in_graph then with_conc else (req, []) :: with_conc

let generate_graph ic : graph =
  let generate_tuple line =
    Scanf.sscanf line "Step %s must be finished before step %s can begin." (fun r c -> (r, c)) in
  let rec aux g =
    try
      begin
        input_line ic
        |> generate_tuple
        |> insert_in_graph g
        |> aux
      end
    with End_of_file -> g
  in
  aux []

let pass_time (countdown : countdown) task_list =
  let update_countdown c task =
    let matches, doesnt_match = List.partition (fun (a, _) -> a = task) c in
    let (_, time) = List.hd matches in
    if time > 0 then (task, time - 1) :: doesnt_match
    else (task, 0) :: doesnt_match
  in
  task_list
  |> List.fold_left update_countdown countdown

let rec work graph time countdown =
  if countdown_sum countdown = 0 then time else
    available_vertices graph countdown
    |> get_first 5
    |> pass_time countdown
    |> work graph (time + 1)

let main () =
  let ic = open_in "input.txt" in
  let graph = generate_graph ic in
  work graph 0 (generate_countdown graph)
  |> string_of_int
  |> print_endline
  ;
  close_in ic

let () =
  main()
