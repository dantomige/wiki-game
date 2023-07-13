open! Core

(* [get_credits] should take the contents of an IMDB page for an actor and
   return a list of strings containing that actor's main credits. *)
let get_credits contents : string list =
  let open Soup in
  let nodes_containing_credits =
    parse contents
    $$ "a[class]"
    |> to_list
    |> List.filter ~f:(fun a_node ->
         String.equal
           (R.attribute "class" a_node)
           "ipc-primary-image-list-card__title")
  in
  List.dedup_and_sort
    (List.map nodes_containing_credits ~f:(fun a_node ->
       texts a_node |> String.concat ~sep:"" |> String.strip))
    ~compare:String.compare
;;

let print_credits_command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:
      "given an IMDB page for an actor, print out a list of their main \
       credits"
    [%map_open
      let how_to_fetch, resource = File_fetcher.param in
      fun () ->
        let contents = File_fetcher.fetch_exn how_to_fetch ~resource in
        List.iter (get_credits contents) ~f:print_endline]
;;

let command =
  Command.group
    ~summary:"imdb commands"
    [ "print-credits", print_credits_command ]
;;
