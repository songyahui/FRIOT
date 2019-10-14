open String

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

let rec showTerms t = 
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
        | Ttimes (es1, t) -> true (*TODO: need to call the Z3*)
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
    Effect (p , es_eff) ->  (p, Cons (es_eff, es))
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
            if  ev1 == ev then Effect (p, Emp) else Effect (p, Bot)
        (*
        | Omega es1 -> appendEff_ES (derivative (Effect (p, es1)) ev) es
        | ESOr (es1 , es2) -> (p, ESOr (derivative es1 ev, derivative es2 ev))
        | Ttimes (es1, t) -> 
            let pi = PureAnd (Gt (t, 0), p) in
            let efF = derivative es1 ev in 
            let eff_2 = Ttimes (es1, Minus (t, 1)) in
             (pi, efF)
        (*TODO: need to call the Z3*)
        
        (*Cons (es1 , es2) -> (p, Cons (derivative es1 ev, derivative es2 ev))
        *)*)
        
      )
  | Disj (_, _) -> raise ( Foo "derivative exception!")
  ;;

(*----------------------------------------------------
----------------------TESTING-------------------------
----------------------------------------------------*)

let ttest = (Minus ((Var "song"),1));;
let estest = Omega (Event "a");;
let puretest = Gt (ttest, 0) ;;
let testes = Effect (puretest, estest);; 
let testcontext = Delta [testes; testes];;

(*Printf.printf "%s" (showTerms  ttest);;
Printf.printf "%s" (showES estest);;

Printf.printf "%s" (showPure puretest);;

Printf.printf "%s" (showEffect testes);;
Printf.printf "%s" (showContext testcontext );;*)
Printf.printf "%s" ("\n")

let context = Z3.mk_context []
let solver = Z3.Solver.mk_solver context None

let xsy = Z3.Symbol.mk_string context "x"
let x = Z3.Boolean.mk_const context xsy

let () = Z3.Solver.add solver [x]

let main () =
    match Z3.Solver.check solver [] with
    | UNSATISFIABLE -> Printf.printf "unsat\n"
    | UNKNOWN -> Printf.printf "unknown"
    | SATISFIABLE ->
        match Z3.Solver.get_model solver with
        | None -> ()
        | Some model ->
            Printf.printf "%s\n"
                (Z3.Model.to_string model)


let a = main ()

