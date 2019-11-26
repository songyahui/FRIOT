open Ast
open Rewriting
open Parser
open Lexer
open Printf


(*
let () =
  let r = print_endline ("Please type the LHS effect: ");read_line () in
  let lhs = Parser.main Lexer.token (Lexing.from_string r) in
  let s = print_endline ("Please type the RHS effect: "); read_line () in
  let rhs = Parser.main Lexer.token (Lexing.from_string s) in
  print_endline (showEffect lhs);
  print_endline (showEffect rhs);
  printReport lhs rhs ;

  
  ;*)

  let path = "/";;

  

let () = 
    let inputfile = (Sys.getcwd () ^ path ^ Sys.argv.(1)) in 
    let outputfile = (Sys.getcwd ()^ path ^ Sys.argv.(2)) in
    let ic = open_in inputfile in
    try 
      let line = input_line ic in  (* 从输入通道读入一行并丢弃'\n'字符 *)
      let EE (lhs, rhs) = Parser.main Lexer.token (Lexing.from_string line) in
      let result = printReport lhs rhs in 
      let oc = open_out outputfile in    (* 新建或修改文件,返回通道 *)
      fprintf oc "%s\n" result;   (* 写一些东西 *)
      close_out oc;                (* 写入并关闭通道 *)
      print_string result;
      flush stdout;                (* 现在写入默认设备 *)
      close_in ic                  (* 关闭输入通道 *) 
  
    with e ->                      (* 一些不可预见的异常发生 *)
      close_in_noerr ic;           (* 紧急关闭 *)
      raise e                      (* 以出错的形式退出: 文件已关闭,但通道没有写入东西 *)
  
   ;;

(*

ocamllex lexer.mll
menhir parser.mly
ocamlc -c ast.mli
ocamlc -c parser.mli
ocamlc -c parser.ml
ocamlc -c lexer.ml
ocamlc -c EffectParse.ml
ocamlc -o regex parser.cmo lexer.cmo EffectParse.cmo

ocamllex lexer.mll
menhir parser.mly
ocamlc -c ast.mli
ocamlc -c parser.mli
ocamlc -c parser.ml
ocamlc -c lexer.ml
ocamlfind ocamlopt -o trs -cclib -lstdc++ -thread -package z3 -linkpkg Tree.ml Rewriting.ml parser.ml lexer.ml EffectParse.ml


1. containment, entilment, term rewriting system.
2. not complete.



*)
