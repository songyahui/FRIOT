open Ast
open Rewriting
open Parser
open Lexer



let () =
  let r = print_endline ("Please type the LHS effect: ");read_line () in
  let lhs = Parser.main Lexer.token (Lexing.from_string r) in
  let s = print_endline ("Please type the RHS effect: "); read_line () in
  let rhs = Parser.main Lexer.token (Lexing.from_string s) in
  print_endline (showEffect lhs);
  print_endline (showEffect rhs);
  printReport lhs rhs ;

  
  ;


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
ocamlfind ocamlopt -o trs -package z3 -linkpkg Tree.ml Rewriting.ml Parser.ml Lexer.ml EffectParse.ml

*)