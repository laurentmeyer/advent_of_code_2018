let main () =
  let ic = open_in "input.txt" in
  let rec aux acc =
    try
      input_line ic
      |> int_of_string
      |> (+) acc
      |> aux
    with End_of_file -> acc
  in
  aux 0
  |> string_of_int
  |> print_endline ;
  close_in ic


let () =
  main()