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


type context =  (effect * effect) list

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

let showEntailmentEff eff1 eff2 = showEffect eff1 ^ " |- "  ^ showEffect eff2

let showEntailmentES es1 es2 = showES es1 ^ " |- "  ^ showES es2


let rec showContext (d:context) = 
  match d with
    [] -> ""
  | (eff1, eff2)::rest -> (showEntailmentEff eff1 eff2 )^ ("\n") ^ showContext rest
  ;;



(*----------------------------------------------------
------------------Utility Functions------------------
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
  let z3_context = Z3.mk_context [] in
  let constrains = getZ3ConstrainFromPure pi z3_context [] in
  let solver = Z3.Solver.mk_solver z3_context None in
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
      let normalES1 = normalES es1 in
      (match (normalES1, es2) with 
        (Emp, _) -> es2
      | (Bot, _) -> Bot
      | (Omega _, _ ) -> normalES1
      | (normal_es1, _) -> Cons (normal_es1, normalES es2)
      ;)
  | ESOr (es1, es2) -> 
      (match (normalES es1, normalES es2) with 
        (Bot, Bot) -> Bot
      | (Bot, norml_es2) -> norml_es2
      | (norml_es1, Bot) -> norml_es1
      | (norml_es1, norml_es2) -> ESOr (norml_es1, norml_es2)
      ;)
  | Ttimes (es1, terms) -> 
      let normalInside = normalES es1 in 
      (match normalInside with
        Emp -> Emp
      | _ ->  Ttimes (normalInside, terms))
  | Omega es1 -> 
      let normalInside = normalES es1 in 
      (match normalInside with
        Emp -> Emp
      | _ ->  Omega normalInside)
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
    Event ev ->  ev
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
----------------------CONTAINMENT--------------------
----------------------------------------------------*)

let rec compareTerm term1 term2 = 
  match (term1, term2) with 
    (Var s1, Var s2) -> true
  | (Plus (tIn1, num1), Plus (tIn2, num2)) -> compareTerm tIn1 tIn2 && num1 == num2
  | (Minus (tIn1, num1), Minus (tIn2, num2)) -> compareTerm tIn1 tIn2 && num1 == num2
  | _ -> false 
  ;;
let rec stricTcompareTerm term1 term2 = 
  match (term1, term2) with 
    (Var s1, Var s2) -> String.compare s1 s2 == 0
  | (Plus (tIn1, num1), Plus (tIn2, num2)) -> compareTerm tIn1 tIn2 && num1 == num2
  | (Minus (tIn1, num1), Minus (tIn2, num2)) -> compareTerm tIn1 tIn2 && num1 == num2
  | _ -> false 
  ;;

let rec compareES es1 es2 = 
  match (es1, es2) with 
    (Bot, Bot) -> true
  | (Emp, Emp) -> true
  | (Event s1, Event s2) -> s1 == s2
  | (Cons (es1L, es1R), Cons (es2L, es2R)) -> (compareES es1L es2L) && (compareES es1R es2R)
  | (ESOr (es1L, es1R), ESOr (es2L, es2R)) -> 
      let one = ((compareES es1L es2L) && (compareES es1R es2R)) in
      let two =  ((compareES es1L es2R) && (compareES es1R es2L)) in 
      one || two
  | (Omega esL, Omega esR) ->compareES esL esR
  | (Ttimes (esL, termL), Ttimes (esR, termR)) -> 
      let insideEq = (compareES esL esR) in
      let termEq = compareTerm termL termR in
      insideEq && termEq
  | _ -> false
;;

let rec compareEff eff1 eff2 =
  match (eff1, eff2) with
    (Effect (pi1, es1), Effect (pi2, es2)) -> compareES es1 es2
  | (Disj (eff11, eff12), Disj (eff21, eff22)) -> 
      let one =  (compareEff eff11  eff21) && (compareEff eff12  eff22) in
      let two =  (compareEff eff11  eff22) && (compareEff eff12  eff21 ) in
      one || two
  | _ -> false
  ;;

let rec reoccur effL effR delta  = 
  match delta with 
  | [] -> false
  | (eff1, eff2) :: rest -> 
      if (compareEff effL eff1 && compareEff effR  eff2) then true 
      else reoccur effL effR rest (*REOCCUR*) 
  ;;

let rec addConstrain effect addPi =
  match effect with
    Effect (pi, eff) -> Effect (PureAnd (pi, addPi), eff)
  | Disj (effL1, effL2) -> Disj (addConstrain effL1 addPi, addConstrain effL2 addPi)
  ;;

let entailConstrains pi1 pi2 = askZ3 (PureAnd (pi1, pi2)) ;;

let rec getPureFromEffect effect = 
  match effect with
    Effect (pi, _) -> pi
  | Disj (eff1, eff2) -> PureOr ((getPureFromEffect eff1), (getPureFromEffect eff2))
  ;;

let rec getAllVarFromES es = 
  match es with
  | Ttimes (_, Var s) -> [s]
  | Ttimes (_, Plus (Var s, _ )) -> [s]
  | Ttimes (_, Minus (Var s, _ )) -> [s]
  | Cons (es1, es2) -> append (getAllVarFromES es1 ) (getAllVarFromES es2 ) 
  | ESOr (es1, es2) -> append (getAllVarFromES es1 ) (getAllVarFromES es2 ) 
  | Omega (esIn) -> getAllVarFromES esIn
  | _ -> []
  ;;

let rec getAllVarFromEff effect = 
  match effect with 
    Effect (pi, es) -> getAllVarFromES es
  | Disj (eff1, eff2) -> append (getAllVarFromEff eff1) (getAllVarFromEff eff2)
  ;;

let getAllVarFromDelta delta acc =  
  match delta with 
    [] -> acc
  | (eff1, eff2)::rest -> append acc (append (getAllVarFromEff eff1 ) (getAllVarFromEff eff2 ) )
  ;;

let freeVar = ["a"; "b"; "c"; "d";"e";"f";"g";"h";"i"];;

let rec exist li ele = 
  match li with 
    [] -> false 
  | x :: xs -> if (String.compare x ele) == 0 then true else exist xs ele
  ;;

let rec getAfreeVar delta  =
  let bounded = getAllVarFromDelta delta [] in
  let rec findOne li = 
    match li with 
        [] -> raise ( Foo "freeVar list too small exception!")
      | x :: xs -> if (exist bounded x) == true then findOne xs else x
  in
  findOne freeVar
;;

let rec substituteES es termOrigin termNew = 
  match es with 
  | Ttimes (es, term) -> if (stricTcompareTerm term termOrigin) == true then Ttimes (es, termNew) else es
  | Cons (es1, es2) -> Cons (substituteES es1 termOrigin termNew ,substituteES es2 termOrigin termNew ) 
  | ESOr (es1, es2) -> Cons (substituteES es1 termOrigin termNew ,substituteES es2 termOrigin termNew ) 
  | Omega (es) -> Omega (substituteES es termOrigin termNew)
  | _ -> es
  ;;

let rec substituteEff (effect:effect) (termOrigin:terms) (termNew:terms) = 
  match effect with 
    Effect (pi, es) -> Effect (pi, substituteES es termOrigin termNew) 
  | Disj (eff1, eff2) -> Disj (substituteEff eff1 termOrigin termNew , substituteEff eff2 termOrigin termNew ) 
  ;;

let isEmp effect = 
  match effect with
    Effect (_ , Emp) -> true
  | _ -> false 

let rec containment (effL:effect) (effR:effect) (delta:context)= 
  let unfold esL delta effL effR normalFormL normalFormR= 
    (let fstL = fst esL in 
    let deltaNew = append delta [(effL, effR)] in
    let derivL = derivative normalFormL fstL in
    let derivR = derivative normalFormR fstL in
    containment derivL derivR deltaNew  )
  in 
  let normalFormL = normalEffect effL in 
  let normalFormR = normalEffect effR in
  
  match normalFormL with
    Effect (piL, esL) -> 
    
      (*if (nullable normalFormL) == true && (nullable normalFormR) == false then false (*"Disprove-Emp"*)*)
      if (isEmp normalFormL) == true && (isEmp normalFormR) == false then false (*"Disprove-Emp"*)
      else 
        (match normalFormR with 
          Effect (piR, Emp) ->  if entailConstrains piL piR then true (*"Prove-Frame"*)
                                else false (*"Disprove-Frame"*)
        |  _ -> 
          if (reoccur normalFormL normalFormR delta) == true 
          then entailConstrains piL (getPureFromEffect normalFormR)
             (*"reoccur", TODO maybe put subtitution here as well*) 
          else (match esL with
                | ESOr (es1, es2) -> (containment (Effect(piL, es1)) effR delta) && (containment (Effect(piL, es2)) effR delta)
                | Ttimes (esIn, term) -> 
                    (match term with 
                      Var s -> 
                        (match  entailConstrains piL (Eq (Var s, 0) ) with 
                          true -> (*CASE SPLIT*) 
                            let zeroCase = PureAnd (piL, Eq (Var s, 0) ) in 
                            let nonZeroCase = PureAnd (piL, Gt (Var s, 0) ) in 
                            let leftZero = addConstrain (Effect(piL, Emp)) zeroCase in
                            let rightZero = addConstrain normalFormR zeroCase in
                            let leftNonZero = addConstrain normalFormL nonZeroCase in
                            let rightNonZero = addConstrain normalFormR nonZeroCase in
                            (containment leftZero rightZero delta) || (containment leftNonZero rightNonZero delta)
                        | false -> (*UNFOLD*)unfold esL delta effL effR normalFormL normalFormR
                        )
                    | Plus  (Var t, num) -> 
                        let newVar = getAfreeVar delta in 
                        let lhs = substituteEff normalFormL  (Plus  (Var t, num)) (Var newVar) in
                        let rhs = substituteEff normalFormR  (Plus  (Var t, num)) (Var newVar) in
                        containment lhs rhs delta
                    | Minus (Var t, num) -> 
                        let newVar = getAfreeVar delta in 
                        let lhs = substituteEff normalFormL  (Minus  (Var t, num)) (Var newVar) in
                        let rhs = substituteEff normalFormR  (Minus  (Var t, num)) (Var newVar) in
                        containment lhs rhs delta
                    | _ -> raise ( Foo "term is too complicated exception!")
                    )
                | Cons (Ttimes (esIn, term), restES) -> 
                    (match term with 
                      Var s -> 
                        (match  entailConstrains piL (Eq (Var s, 0) ) with 
                          true -> (*CASE SPLIT*) 
                            let zeroCase = PureAnd (piL, Eq (Var s, 0) ) in 
                            let nonZeroCase = PureAnd (piL, Gt (Var s, 0) ) in 
                            let leftZero = addConstrain (Effect(piL, restES)) zeroCase in
                            let rightZero = addConstrain normalFormR zeroCase in
                            let leftNonZero = addConstrain normalFormL nonZeroCase in
                            let rightNonZero = addConstrain normalFormR nonZeroCase in
                            (containment leftZero rightZero delta) || (containment leftNonZero rightNonZero delta)
                        | false -> (*UNFOLD*)unfold esL delta effL effR normalFormL normalFormR
                        )
                    | Plus  (Var t, num) -> 
                        let newVar = getAfreeVar delta in 
                        let lhs = substituteEff normalFormL  (Plus  (Var t, num)) (Var newVar) in
                        let rhs = substituteEff normalFormR  (Plus  (Var t, num)) (Var newVar) in

                        containment lhs rhs delta
                    | Minus (Var t, num) -> 
                        let newVar = getAfreeVar delta in 
                        let lhs = substituteEff normalFormL  (Minus  (Var t, num)) (Var newVar) in
                        let rhs = substituteEff normalFormR  (Minus  (Var t, num)) (Var newVar) in
                        containment lhs rhs delta
                    | _ -> raise ( Foo "term is too complicated exception!")
                    )
                | _ -> (*UNFOLD*)unfold esL delta effL effR normalFormL normalFormR
                ) 
        ;)       

  | Disj (effL1, effL2) -> (containment effL1 effR delta) && (containment effL2 effR delta)
  ;;
  

(*----------------------------------------------------
----------------------TESTING-------------------------
----------------------------------------------------*)

let ttest = (Plus ((Var "song"),1));;
let ttest1 = (Var "t");;
let estest = ESOr (Cons (Ttimes ((Event "a"), Var "t"),  (Event "a")), Cons ((Event "a"),(Event "b")));;
let puretest =  Eq (ttest1, 0);;
let testes = Effect (puretest, estest);; 
let testcontext =  [testes; testes];;
let testD = derivative testes ( "a");;
let leftEff = Effect (TRUE, ESOr (Omega (Event "a"), Omega (Event "b"))) ;;
let rightEff = Effect (TRUE, Omega (Event "b")) ;;
let leftEff1 = Effect (TRUE, Cons (Event "a", Cons (Event "b", Event "c"))) ;;
let rightEff2 = Effect (TRUE, Cons (Event "a", Cons (Event "d", Event "c"))) ;;
let lhsss = Effect (TRUE, Cons (Ttimes ((Event "a"), Var "t"), Event "c"));;
let rhsss = Effect (TRUE, Omega ((Event "a")));;
let check = containment  rhsss rhsss [] ;;

(*Printf.printf "%s" (showTerms  ttest);;
Printf.printf "%s" (showES estest);;

Printf.printf "%s" (showPure puretest);;

Printf.printf "%s" (showEffect testes);;
Printf.printf "%s" (showContext testcontext );;*)
let a = askZ3 puretest ;;

Printf.printf "%b" check;;

Printf.printf "%s" (showEntailmentEff  rhsss rhsss);;

Printf.printf "%s" ("\n");;
