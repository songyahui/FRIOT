(*
   Created by Martin Jambon and placed in the Public Domain on June 1, 2019.

   Print a tree or a DAG as tree, similarly to the 'tree' command.
*)
open Printf


let rec iter f = function
  | [] -> ()
  | [x] ->
      f true x
  | x :: tl ->
      f false x;
      iter f tl

let to_buffer ?(line_prefix = "") ~get_name ~get_children buf x =
  let rec print_root indent x =
    bprintf buf "%s\n" (get_name x);
    let children = get_children x in
    iter (print_child indent) children
  and print_child indent is_last x =
    let line =
      if is_last then
        "└── "
      else
        "├── "
    in
    bprintf buf "%s%s" indent line;
    let extra_indent =
      if is_last then
        "    "
      else
        "│   "
    in
    print_root (indent ^ extra_indent) x
  in
  Buffer.add_string buf line_prefix;
  print_root line_prefix x

let printTree ?line_prefix ~get_name ~get_children x =
  let buf = Buffer.create 1000 in
  to_buffer ?line_prefix ~get_name ~get_children buf x;
  Buffer.contents buf

type binary_tree =
  | Node of string * (binary_tree  list )
  | Leaf


(******************testing********************)

let root = Node("root", [Leaf;Leaf]);;

let get_name = function
    | Leaf -> "."
    | Node (name, li) -> name;;

let get_children = function
    | Leaf -> []
    | Node (_, li) -> List.filter ((<>) Leaf) li;;

let shared_node =
    Node (
      "hello",
      [Node ("world", [Leaf; Leaf]);
      Node ("you", [Leaf; Leaf]);
      Node ("you", [Leaf; Leaf])]
    );;

let tree =
    Node (
      "root",
      [Node (
        "Mr. Poopypants",
        [Node (
          "something something",
          [shared_node;
          Leaf]
        );
        Node (
          "Ms. Poopypants",
          [Leaf;
          Leaf]
        )]
      );
      Leaf]
    );;

    
(*let () =
  let result = printTree ~line_prefix:"* " ~get_name ~get_children shared_node 
  in
  print_string result;
  flush stdout;;*)






