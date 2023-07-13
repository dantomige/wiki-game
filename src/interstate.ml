open! Core
module City = String
module Interstate = String

module G =
  Graph.Imperative.Graph.ConcreteLabeled
    (City)
    (struct
      include Interstate

      let default = ""
    end)

(* module G = Graph.Imperative.Digraph.ConcreteBidirectionalLabeled *)
module Dot = Graph.Graphviz.Dot (struct
  include G

  (* These functions can be changed to tweak the appearance of the generated
     graph. Check out the ocamlgraph graphviz API
     (https://github.com/backtracking/ocamlgraph/blob/master/src/graphviz.mli)
     for examples of what values can be set here. *)
  let edge_attributes _ = [ `Dir `None ]
  let default_edge_attributes _ = []
  let get_subgraph _ = None
  let vertex_attributes v = [ `Shape `Box; `Label v; `Fillcolor 1000 ]
  let vertex_name v = v
  let default_vertex_attributes _ = []
  let graph_attributes _ = []
end)

(* We separate out the [Network] module to represent our social network in
   OCaml types. *)
module Network = struct
  (* We can represent our social network graph as a set of connections, where
     a connection represents a friendship between two people. *)
  module Connection = struct
    module T = struct
      type t = City.t * Interstate.t * City.t [@@deriving compare, sexp]
    end

    (* This funky syntax is necessary to implement sets of [Connection.t]s.
       This is needed to defined our [Network.t] type later. Using this
       [Comparable.Make] functor also gives us immutable maps, which might
       come in handy later. *)
    include Comparable.Make (T)

    (* let of_string s = match String.split s ~on:',' with | [ city1;
       interstate; city2 ] -> Some ( City.of_string city1 ,
       Interstate.of_string interstate , City.of_string city2 ) | _ -> None
       ;; *)

    let of_string_line s =
      let interstate_city_list = String.split s ~on:',' in
      let interstate = List.hd_exn interstate_city_list in
      let cities = List.tl_exn interstate_city_list in
      let rec get_combinatorial_city_pairs cities =
        match cities with
        | [] -> []
        | first_city :: other_cities ->
          List.fold cities ~init:[] ~f:(fun city_tuple_list other_city ->
            city_tuple_list
            @ [ ( City.of_string first_city
                , Interstate.of_string interstate
                , City.of_string other_city )
              ])
          @ get_combinatorial_city_pairs other_cities
      in
      get_combinatorial_city_pairs cities
    ;;
  end

  type t = Connection.Set.t [@@deriving sexp_of]

  let of_file input_file =
    let connections =
      In_channel.read_lines (File_path.to_string input_file)
      |> List.map ~f:(fun file_line -> Connection.of_string_line file_line)
      |> List.concat
    in
    Connection.Set.of_list connections
  ;;
end

let load_command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:"parse a file listing interstates and serialize graph as a sexp"
    [%map_open
      let input_file =
        flag
          "input"
          (required File_path.arg_type)
          ~doc:
            "FILE a file listing interstates and the cities they go through"
      in
      fun () ->
        let network = Network.of_file input_file in
        (* This special syntax can be used to easily sexp-serialize values
           (whose types have [sexp_of_t] implemented). *)
        printf !"%{sexp: Network.t}\n" network]
;;

let visualize_command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:
      "parse a file listing interstates and generate a graph visualizing \
       the highway network"
    [%map_open
      let input_file =
        flag
          "input"
          (required File_path.arg_type)
          ~doc:
            "FILE a file listing all interstates and the cities they go \
             through"
      and output_file =
        flag
          "output"
          (required File_path.arg_type)
          ~doc:"FILE where to write generated graph"
      in
      fun () ->
        let network = Network.of_file input_file in
        let graph = G.create () in
        Set.iter network ~f:(fun (city1, interstate, city2) ->
          (* [G.add_edge] auomatically adds the endpoints as vertices in the
             graph if they don't already exist. *)
          (* G.add_edge graph city1 interstate city2); *)
          (* open Sig in *)
          let edge = G.E.create city1 interstate city2 in
          G.add_edge_e graph edge);
        Dot.output_graph
          (Out_channel.create (File_path.to_string output_file))
          graph;
        printf !"Done! Wrote dot file to %{File_path}\n%!" output_file]
;;

let command =
  Command.group
    ~summary:"interstate highway commands"
    [ "load", load_command; "visualize", visualize_command ]
;;
