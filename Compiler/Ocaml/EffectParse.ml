open Ast

let rec showTerms = function
  | Var name -> name
  | Plus (t, num) -> (showTerms t) ^ ("+") ^ (string_of_int num)
  | Minus (t, num) -> (showTerms t) ^ ("-") ^ (string_of_int num)
  ;;

let rec showES = function
| Bot -> "_|_"
| Emp -> "emp"
| Event ev -> ev 
| Cons (es1, es2) -> "("^(showES es1) ^ "." ^ (showES es2)^")"
| ESOr (es1, es2) -> "("^(showES es1) ^ "+" ^ (showES es2)^")"
| Ttimes (es, t) -> (showES es) ^ "^" ^ (showTerms t)
| Omega es -> (showES es) ^ "^" ^  "w" 

let rec showPure (p:pure):string = 
  match p with
    TRUE -> "true"
  | FALSE -> "false"
  | Gt (t, num) -> (showTerms t) ^ ">" ^ (string_of_int num)
  | Lt (t, num) -> (showTerms t) ^ "<" ^ (string_of_int num)
  | Eq (t, num) -> (showTerms t) ^ "=" ^ (string_of_int num)
  | PureOr (p1, p2) -> showPure p1 ^ "\\/" ^ showPure p2
  | PureAnd (p1, p2) -> showPure p1 ^ "/\\" ^ showPure p2
  | Neg p -> "~" ^ showPure p
  ;; 

(*To pretty print effects*)
let rec showEffect (e:effect) :string = 
  match e with
    Effect (p, es) -> 
      if p == TRUE then showES es
      else showPure p ^ "/\\" ^ showES es
  | Disj (es1, es2) -> "(" ^ showEffect es1 ^ ")\\/("  ^ showEffect es2^")"
  ;;

let () =
  let s = read_line () in
  let r = Parser.main Lexer.token (Lexing.from_string s) in
  print_endline (showEffect r)


(*

ocamllex lexer.mll
menhir parser.mly
ocamlc -c ast.mli
ocamlc -c parser.mli
ocamlc -c parser.ml
ocamlc -c lexer.ml
ocamlc -c EffectParse.ml
ocamlc -o regex parser.cmo lexer.cmo main.cmo

*)