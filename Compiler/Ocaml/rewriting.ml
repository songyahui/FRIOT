open String
open Z3
open Z3.Symbol
open Z3.Sort
open Z3.Expr
open Z3.Boolean
open Z3.FuncDecl
open Z3.Goal
open Z3.Tactic
open Z3.Tactic.ApplyResult
open Z3.Probe
open Z3.Solver
open Z3.Arithmetic
open Z3.Arithmetic.Integer
open Z3.Arithmetic.Real
open Z3.BitVector
open List


(*----------------------------------------------------
---------------------DATA STRUCTURE-----------------
----------------------------------------------------*)
type terms = Var of string
           | Plus of terms * int
           | Minus of terms * int

type event =  string 

type es = Bot 
        | Emp 
        | Event of event
        | Cons of es * es
        | ESOr of es * es
        | Ttimes of es * terms
        | Omega of es

type pure = TRUE
          | FALSE
          | Gt of terms * int
          | Lt of terms * int
          | Eq of terms * int
          | PureOr of pure * pure
          | PureAnd of pure * pure
          | Neg of pure

type effect = Effect of pure * es
          | Disj of effect * effect

type context = Delta of effect list

(*----------------------------------------------------
----------------------PRINTING------------------------
----------------------------------------------------*)

let rec showTerms (t:terms) = 
  match t with
    Var name -> name
  | Plus (t, num) -> (showTerms t) ^ ("+") ^ (string_of_int num)
  | Minus (t, num) -> (showTerms t) ^ ("-") ^ (string_of_int num)
  ;;
let rec showES es = 
  match es with
    Bot -> "_|_"
  | Emp -> "emp"
  | Event ev -> ev 
  | Cons (es1, es2) -> (showES es1) ^ "." ^ (showES es2)
  | ESOr (es1, es2) -> (showES es1) ^ "+" ^ (showES es2)
  | Ttimes (es, t) -> (showES es) ^ "^" ^ (showTerms t)
  | Omega es -> (showES es) ^ "^" ^  "w" 
  ;;

let rec showPure p = 
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

let rec showEffect e = 
  match e with
    Effect (p, es) -> showPure p ^ "/\\" ^ showES es
  | Disj (es1, es2) -> showEffect es1 ^ "\\/"  ^ showEffect es2
  ;;

let showEntailment es1 es2 = showEffect es1 ^ " |- "  ^ showEffect es2

let rec showContext d = 
  match d with
    Delta [] -> ""
  | Delta (eff1::rest) -> showEffect eff1 ^ ("\n") ^ showContext (Delta rest)
  ;;



(*----------------------------------------------------
----------------------CONTAINMENT--------------------
----------------------------------------------------*)
exception Foo of string

let rec getTermNameFromTerm (t:terms) =
  match t with 
    Var name -> name
  | Plus (ter, num) -> getTermNameFromTerm ter
  | Minus (ter, num) -> getTermNameFromTerm ter
  ;;

let rec getZ3ExprFromTerm (term:terms) ctx = 
  match term with
    Var name -> 
      let name_is = (Symbol.mk_string ctx name) in
      let x_is = Integer.mk_const ctx name_is in
      x_is
  | Plus (t, num) -> 
      let name_is = (Symbol.mk_string ctx (getTermNameFromTerm t))  in
      let x_is = Integer.mk_const ctx name_is in
      let make_plus = Arithmetic.mk_add ctx [ x_is ; (Integer.mk_numeral_i ctx num)] in
      make_plus
  | Minus (t, num) -> 
      let name_is = (Symbol.mk_string ctx (getTermNameFromTerm t))  in
      let x_is = Integer.mk_const ctx name_is in
      let make_minus = Arithmetic.mk_add ctx [ x_is ; (Integer.mk_numeral_i ctx (0 - num))] in
      make_minus
  ;;

let rec getZ3ConstrainFromPure pi ctx acc= 
  match pi with 
    TRUE -> acc 
  | FALSE -> 
      let name_is = (Symbol.mk_string ctx "constructFalse") in
      let x_is = Integer.mk_const ctx name_is in
      let x1 = (mk_gt ctx (x_is) 
          (Integer.mk_numeral_i ctx 0)) in
      let x2 = (mk_eq ctx (x_is) 
          (Integer.mk_numeral_i ctx 0)) in
      append acc [x1;x2]
  | Gt (term, num) -> 
      let expr = getZ3ExprFromTerm term ctx in 
      let assertion = mk_gt ctx expr (Integer.mk_numeral_i ctx num) in
      append acc [assertion]
  | Lt (term, num) -> 
      let expr = getZ3ExprFromTerm term ctx in 
      let assertion = (mk_lt ctx expr (Integer.mk_numeral_i ctx num)) in
      append acc [assertion]
  | Eq (term, num) -> 
      let expr = getZ3ExprFromTerm term ctx in 
      let assertion = (mk_eq ctx expr (Integer.mk_numeral_i ctx num)) in
      append acc [assertion]
  | PureAnd (pi1,pi2) -> 
      let assert1 = getZ3ConstrainFromPure pi1 ctx [] in
      let assert2 = getZ3ConstrainFromPure pi2 ctx [] in
      append (append acc assert1) assert2
  | Neg piN -> 
      let assert1 = getZ3ConstrainFromPure piN ctx [] in
      let makenot = mk_not ctx (List.nth assert1 0)  in
      append acc [makenot]
  | _ ->raise ( Foo "getZ3ConstrainFromPure exception!")
;;

let askZ3 pi = 
  let context = Z3.mk_context [] in
  let constrains = getZ3ConstrainFromPure pi context [] in
  let solver = Z3.Solver.mk_solver context None in
  let () = Z3.Solver.add solver constrains in
    match Z3.Solver.check solver [] with
    | UNSATISFIABLE -> 
      (*Printf.printf "unsat\n" *)
      false
    | UNKNOWN -> 
      (*Printf.printf "unknown"*)
      false
    | SATISFIABLE -> true
        (*match Z3.Solver.get_model solver with
        | None -> ()
        | Some model ->
            Printf.printf "%s\n"
                (Z3.Model.to_string model)*)

let rec normalES es = 
  match es with
    Bot -> es
  | Emp -> es
  | Event ev -> es
  | Cons (es1, es2) -> 
      (match (normalES es1, es2) with 
        (Emp, _) -> es2
      | (Bot, _) -> Bot
      | (normal_es1, _) -> Cons (normal_es1, normalES es2)
      ;)
  | ESOr (es1, es2) -> 
      (match (normalES es1, normalES es2) with 
        (Bot, Bot) -> Bot
      | (Bot, norml_es2) -> norml_es2
      | (norml_es1, Bot) -> norml_es1
      | (norml_es1, norml_es2) -> ESOr (norml_es1, norml_es2)
      ;)
  | Ttimes (es1, terms) -> normalES es1
  | Omega es1 -> normalES es1
  ;;

let rec normalEffect eff =
  match eff with
    Effect (p, es) -> 
      if (askZ3 p) == false then Effect (FALSE,  Bot)
      else if normalES es == Bot then Effect (FALSE,  Bot)
      else Effect (p , normalES es)
  | Disj (eff1, eff2) -> 
      match (normalEffect eff1, normalEffect eff2) with
        (Effect (FALSE,  Bot), _) -> normalEffect eff2
      | (_, Effect (FALSE,  Bot)) -> normalEffect eff1
      | _ -> Disj (normalEffect eff1, normalEffect eff2)
  ;;


let rec nullable eff =
  match eff with
    Effect (p , es) -> 
      (
        match es with
          Bot -> false 
        | Emp -> true
        | Event ev -> false 
        | Cons (es1 , es2) -> nullable (Effect (p,es1)) && nullable (Effect (p,es2))
        | ESOr (es1 , es2) -> nullable (Effect (p,es1)) || nullable (Effect (p,es2))
        | Ttimes (es1, t) -> askZ3 (PureAnd (p, Eq (t,0))) (*TODO: need to call the Z3*)
        | Omega es1 -> false
      )
  | Disj (_, _) -> raise ( Foo "nullable exception!")
  ;;
    
let rec fst es = 
  match es with 
    Event ev -> Event ev
  | Omega es1 -> fst es1
  | Ttimes (es1, t) -> fst es1
  | Cons (es1 , es2) -> fst es1
  | _ -> raise ( Foo "fst exception!")
  ;;

let appendEff_ES eff es = 
  match eff with 
    Effect (p , es_eff) ->  Effect(p, Cons (es_eff, es))
  | _ -> raise ( Foo "appendEff_ES exception!")
  ;;

let rec derivative eff ev =
  match eff with
    Effect (p , es) -> 
      (
        match es with
          Bot -> Effect (FALSE,  Bot)
        | Emp -> Effect (FALSE,  Bot)
        | Event ev1 -> 
            if (String.compare ev1 ev) == 0 then Effect (p, Emp) else Effect (p, Bot)
        | Omega es1 -> appendEff_ES (derivative (Effect (p, es1)) ev) es
        | ESOr (es1 , es2) -> Disj (derivative (Effect (p,es1)) ev, derivative (Effect (p,es2)) ev)
        | Ttimes (es1, t) -> 
            let pi = PureAnd (Gt (t, 1), p) in
            let efF = derivative (Effect (pi, es1)) ev in 
            let esT_minus1 = Ttimes (es1, Minus (t, 1)) in
            appendEff_ES efF esT_minus1
        | Cons (es1 , es2) -> 
            if nullable (Effect (p,es1)) == true 
            then let efF = derivative (Effect (p, es1)) ev in 
                 let effL = appendEff_ES efF es2 in 
                 let effR = derivative (Effect (p, es2)) ev in 
                 Disj (effL, effR)
            else let efF = derivative (Effect (p, es1)) ev in 
                 appendEff_ES efF es2    
      )
  | Disj (eff1, eff2) -> Disj (derivative eff1 ev, derivative eff2 ev)
  ;;

(*----------------------------------------------------
----------------------TESTING-------------------------
----------------------------------------------------*)

let ttest = (Plus ((Var "song"),1));;
let ttest1 = (Var "t");;
let estest = Cons (Ttimes ((Event "a"), Var "t"),  (Event "a"));;
let puretest =  Eq (ttest1, 0);;
let testes = Effect (puretest, estest);; 
let testcontext = Delta [testes; testes];;
let testD = derivative testes ( "a");;

(*Printf.printf "%s" (showTerms  ttest);;
Printf.printf "%s" (showES estest);;

Printf.printf "%s" (showPure puretest);;

Printf.printf "%s" (showEffect testes);;
Printf.printf "%s" (showContext testcontext );;*)
let a = askZ3 puretest ;;

Printf.printf "%s" (showEffect (normalEffect testD));;

Printf.printf "%s" ("\n");;
