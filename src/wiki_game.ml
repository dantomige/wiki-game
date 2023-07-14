open! Core

(* [get_linked_articles] should return a list of wikipedia article lengths
   contained in the input.

   Note that [get_linked_articles] should ONLY return things that look like
   wikipedia articles. In particular, we should discard links that are: -
   Wikipedia pages under special namespaces that are not articles (see
   https://en.wikipedia.org/wiki/Wikipedia:Namespaces) - other Wikipedia
   internal URLs that are not articles - resources that are external to
   Wikipedia - page headers

   One nice think about Wikipedia is that stringent content moderation
   results in uniformity in article format. We can expect that all Wikipedia
   article links parsed from a Wikipedia page will have the form
   "/wiki/<TITLE>". *)

let get_linked_articles contents : string list =
  let open Soup in
  let all_links =
    parse contents
    $$ "a"
    |> to_list
    |> List.filter_map ~f:(fun a_node ->
         let href_string_of_node = R.attribute "href" a_node in
         if String.is_prefix ~prefix:"/wiki" href_string_of_node
         then (
           match Wikipedia_namespace.namespace href_string_of_node with
           | None -> Some href_string_of_node
           | Some _ -> None)
         else None)
  in
  List.dedup_and_sort all_links ~compare:String.compare
;;

let print_links_command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:"Print all of the valid wiki page links on a page"
    [%map_open
      let how_to_fetch, resource = File_fetcher.param in
      fun () ->
        let contents = File_fetcher.fetch_exn how_to_fetch ~resource in
        List.iter (get_linked_articles contents) ~f:print_endline]
;;

(* [visualize] should explore all linked articles up to a distance of
   [max_depth] away from the given [origin] article, and output the result as
   a DOT file. It should use the [how_to_fetch] argument along with
   [File_fetcher] to fetch the articles so that the implementation can be
   tested locally on the small dataset in the ../resources/wiki directory. *)
module G = Graph.Imperative.Graph.Concrete (String)

module Dot = Graph.Graphviz.Dot (struct
  include G

  (* These functions can be changed to tweak the appearance of the generated
     graph. Check out the ocamlgraph graphviz API
     (https://github.com/backtracking/ocamlgraph/blob/master/src/graphviz.mli)
     for examples of what values can be set here. *)
  let edge_attributes _ = [ `Dir `Back ]
  let default_edge_attributes _ = []
  let get_subgraph _ = None
  let vertex_attributes v = [ `Shape `Box; `Label v; `Fillcolor 1000 ]
  let vertex_name v = v
  let default_vertex_attributes _ = []
  let graph_attributes _ = []
end)

(* BFS given the html string*)
let rec dfs_from_head head depth graph visited how_to_fetch =
  if depth > 0
  then (
    let head_title = Lambda_soup_utilities.get_title head in
    Hash_set.add visited head_title;
    let neighbors_html =
      List.map (get_linked_articles head) ~f:(fun url ->
        File_fetcher.fetch_exn how_to_fetch ~resource:url)
    in
    List.iter neighbors_html ~f:(fun neighbor_html ->
      G.add_edge
        graph
        head_title
        (Lambda_soup_utilities.get_title neighbor_html));
    List.iter neighbors_html ~f:(fun neighbor_html ->
      if not
           (Hash_set.mem
              visited
              (Lambda_soup_utilities.get_title neighbor_html))
      then dfs_from_head neighbor_html (depth - 1) graph visited how_to_fetch))
;;

let visualize ?(max_depth = 3) ~origin ~output_file ~how_to_fetch () : unit =
  let graph = G.create () in
  let visited_set = String.Hash_set.create () in
  let head_html_string =
    File_fetcher.fetch_exn how_to_fetch ~resource:origin
  in
  dfs_from_head head_html_string max_depth graph visited_set how_to_fetch;
  Dot.output_graph
    (Out_channel.create (File_path.to_string output_file))
    graph
;;

let visualize_command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:
      "parse a file listing interstates and generate a graph visualizing \
       the highway network"
    [%map_open
      let how_to_fetch = File_fetcher.How_to_fetch.param
      and origin = flag "origin" (required string) ~doc:" the starting page"
      and max_depth =
        flag
          "max-depth"
          (optional_with_default 10 int)
          ~doc:"INT maximum length of path to search for (default 10)"
      and output_file =
        flag
          "output"
          (required File_path.arg_type)
          ~doc:"FILE where to write generated graph"
      in
      fun () ->
        visualize ~max_depth ~origin ~output_file ~how_to_fetch ();
        printf !"Done! Wrote dot file to %{File_path}\n%!" output_file]
;;

(* [find_path] should attempt to find a path between the origin article and
   the destination article via linked articles.

   [find_path] should use the [how_to_fetch] argument along with
   [File_fetcher] to fetch the articles so that the implementation can be
   tested locally on the small dataset in the ../resources/wiki directory.

   [max_depth] is useful to limit the time the program spends exploring the
   graph. *)

let rec dfs_url current_url depth destination how_to_fetch ?(visited = String.Hash_set.create ()) ()=
  if depth >= 0
  then
    Hash_set.add visited current_url;
    if String.equal current_url destination
    then Some [ Lambda_soup_utilities.get_title current_url ] (*Recursion*)
    else 
      let current_file = File_fetcher.fetch_exn how_to_fetch ~resource: current_url in 
      let neighbors_urls = get_linked_articles current_file in 
      List.fold neighbors_urls ~init:None ~f:(fun current_solution_path neighbor_url -> 
        let neighbor_solution_path = 
          (if not Hash_set.mem visited neighbors_urls
          then 
            dfs_url neighbors_urls (depth - 1) destination how_to_fetch visited ()
            |> Option.map ~f:(fun resulting_path -> current_url :: resulting_path)
          else None) 
        in 
        match current_solution_path, neighbor_solution_path with
        | None, None -> None
        | Some current_path, None -> Some current_path
        | None, Some neighbor_path -> Some neighbor_path
        | Some current_path, Some neighbor_path -> (
        if (List.length current_path) > (List.length neighbor_path)
          then Some neighbor_path
          else Some current_path
        )
      )
  else None
;;

let convert_url_path_to_title_path url_path = 
  List.map url_path ~f:(fun url -> Lambda_soup_utilities.get_title (File_fetcher.fetch_exn url))
;;

let find_path ?(max_depth = 3) ~origin ~destination ~how_to_fetch () =
  let url_path = dfs_url origin max_depth destination how_to_fetch ()
  match url_path with
  | None -> None
  | Some path -> Some (convert_url_path_to_title_path path)
  (* ignore (max_depth : int);
  ignore (origin : string);
  ignore (destination : string);
  ignore (how_to_fetch : File_fetcher.How_to_fetch.t);
  failwith "TODO" *)
;;

let find_path_command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:
      "Play wiki game by finding a link between the origin and destination \
       pages"
    [%map_open
      let how_to_fetch = File_fetcher.How_to_fetch.param
      and origin = flag "origin" (required string) ~doc:" the starting page"
      and destination =
        flag "destination" (required string) ~doc:" the destination page"
      and max_depth =
        flag
          "max-depth"
          (optional_with_default 10 int)
          ~doc:"INT maximum length of path to search for (default 10)"
      in
      fun () ->
        match find_path ~max_depth ~origin ~destination ~how_to_fetch () with
        | None -> print_endline "No path found!"
        | Some trace -> List.iter trace ~f:print_endline]
;;

let command =
  Command.group
    ~summary:"wikipedia game commands"
    [ "print-links", print_links_command
    ; "visualize", visualize_command
    ; "find-path", find_path_command
    ]
;;
