open! Core

module Position = struct
  type t =
    { row : int
    ; col : int
    }
  [@@deriving compare, sexp, hash]

  let row { row; _ } = row
  let col { row = _; col } = col

  let ( + ) { row = row1; col = col1 } { row = row2; col = col2 } =
    { row = row1 + row2; col = col1 + col2 }
  ;;
end

let get_board_element board position =
  let row = Position.row position in
  let col = Position.col position in
  Array.get (Array.get board row) col
;;

let get_neighbors board curr_position =
  let height = Array.length board in
  let width = Array.length (Array.get board 0) in
  let position_deltas =
    [ { Position.row = 1; col = 0 }
    ; { Position.row = -1; col = 0 }
    ; { Position.row = 0; col = 1 }
    ; { Position.row = 0; col = -1 }
    ]
  in
  List.fold position_deltas ~init:[] ~f:(fun current_neighbors pos_dx ->
    let new_position = Position.(curr_position + pos_dx) in
    let new_row = Position.row new_position in
    let new_col = Position.col new_position in
    if new_col < width
       && new_col >= 0
       && new_row < height
       && new_row >= 0
       && not (Char.equal (get_board_element board new_position) '#')
    then (
      current_neighbors @ [ new_position ])
    else current_neighbors)
;;

let create_2dboard input_file =
  let outer_array =
    Array.of_list (In_channel.read_lines (File_path.to_string input_file))
  in
  Array.map outer_array ~f:(fun file_line ->
    Array.of_list (String.to_list file_line))
;;

let rec maze_dfs
  board
  ?(starting_position = { Position.row = 1; col = 0 })
  ?(visited = Hash_set.create (module Position))
  ()
  =
  (* print_s [%message (starting_position : Position.t)]; *)
  Hash_set.add visited starting_position;
  (*Base Case*)
  if Char.equal (get_board_element board starting_position) 'E'
  then Some [ starting_position ] (*Recursive Case*)
  else (
    let adjacent_positions = get_neighbors board starting_position in
    let path =
      (*Checking each of the adjacent positions*)
      List.fold
        adjacent_positions
        ~init:None
        ~f:(fun current_solution_path other_position ->
        (*Matching on if there has been a solution found.*)
        match current_solution_path with
        | Some path -> Some path (*If there is not a solution*)
        | None ->
          if not (Hash_set.mem visited other_position)
          then
            maze_dfs board ~starting_position:other_position ~visited ()
            |> Option.map ~f:(fun path -> starting_position :: path)
          else None)
    in
    path)
;;

let solve input_file =
  let board = create_2dboard input_file in
  maze_dfs board ()
;;

let solve_command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:"parse a file containing a maze and find a solution"
    [%map_open
      let input_file =
        flag
          "input"
          (required File_path.arg_type)
          ~doc:"FILE a file containing a maze"
      in
      fun () ->
        let search_result = solve input_file in
        match search_result with
        | None -> printf !"There is no solution!"
        | Some solution ->
          [%sexp (solution : Position.t list)]
          |> Sexp.to_string_hum
          |> print_endline]
;;

let command =
  Command.group ~summary:"maze commands" [ "solve", solve_command ]
;;
