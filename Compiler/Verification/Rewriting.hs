module Verification.Rewriting where
import Data.Tree
import Data.List

type ErrorMsg = String

type Residue = ES

data Result = 
    OK (Tree String) Residue
    | Error (Tree String)
    deriving (Show, Eq)

data Pure 
    = TRUE
    | FALSE
    | Gt String Int
    | Lt String Int
    | Eq String Int
    | AndCon Pure Pure
    | OrCon Pure Pure
    | Neg Pure
    deriving (Show, Eq)

data Term 
    = Iden String
    | Add Term Int
    | Minus Term Int
    deriving (Show, Eq)

data ES 
    = Bottom 
    | Empty
    | Event String
    | Dot ES ES
    | OR ES ES 
    | Ttimes ES Term  -- assuming T cannot be negetive
    | Omega ES 
    deriving (Show, Eq)


data Effect = 
     Single (Pure, ES)
    | AndE Effect Effect
    | OrE  Effect Effect
    --- quantifer free for now...
type Env =  [(Effect, Effect)] 


printES :: ES -> String
printES effect =
    case effect of 
        Bottom -> "_|_"
        Empty -> "emp"
        Event str -> str
        Dot es1 es2 -> "(" ++ (printES es1) ++ ". "++(printES es2) ++")"
        OR es1 es2 -> "(" ++ (printES es1) ++ " + "++(printES es2) ++")"
        Ttimes es t -> (printES es) ++ "^" ++ (printTerm t)
        Omega es -> (printES es) ++ "^w"

printPure :: Pure -> String
printPure con =
    case con of 
        TRUE -> "(true)"
        FALSE -> "(false)"
        Gt str num -> "(" ++ str ++ ">" ++ show num ++ ")"
        Lt str num -> "(" ++str ++ "<" ++ show num ++ ")"
        Eq str num -> "(" ++str ++ "=" ++ show num ++ ")"
        AndCon p1 p2 -> "(" ++(printPure p1) ++ "/\\" ++  (printPure p2) ++ ")"
        OrCon  p1 p2 -> "(" ++(printPure p1) ++ "\\/" ++  (printPure p2) ++ ")"
        Neg p -> "~" ++ ( printPure p)

printTerm ::Term -> String
printTerm t = 
    case t of 
        Iden str -> str
        Add t n -> "(" ++ (printTerm t) ++ "+"++(show n) ++")"
        Minus t n -> "(" ++ (printTerm t) ++ "-"++(show n) ++")"
        

printEffect :: Effect ->String
printEffect effect =
    case effect of 
        (p,es) -> printPure p ++ "/\\" ++ printES es
        AndE e1 e2 -> printEffect e1 ++ "/\\" ++ printEffect e2
        OrE e1 e2 -> printEffect e1 ++ "\\/" ++ printEffect e2


printEntailCondEff :: Effect -> Effect -> String
printEntailCondEff eff1 eff2 = 
    (printEffect eff1) ++ " |- " ++ (printEffect eff2)


first :: ES -> ES
first es = 
    case es of 
        Event ev -> es
        Dot es1 es2 -> first es1
        Omega es -> first es
        Ttimes es t -> first es
        otherwise -> Bottom

nullable :: Effect -> Bool
nullable (pi, es)  =
    case es of 
        Bottom -> False
        Empty -> True
        Event _ -> False
        Omega _ -> False
        Dot es1 es2 -> (nullable (pi, es1)) && (nullable (pi, es2))
        OR es1 es2 -> (nullable (pi, es1)) || (nullable (pi, es2))
        Ttimes es' t -> 
            if pi == TRUE then True
            else False
            -- call Z3, see if t = 0 implies pi (*---STUCK---*)

concat_Eff_Es :: Effect -> ES -> Effect
concat_Eff_Es (Single (pi, es)) es1 = Single (pi, Dot es es1)

choice_Eff_Eff :: Effect -> Effect -> Effect
choice_Eff_Eff (Single (p1, es1)) (Single (p2, es2)) = OrE 


normal :: Effect -> Effect
normal (pi, es) =
     case es of
        OR r s -> 
            if r == s then 
                normal (pi, r)
            else if r == Bottom then  
                normal (pi, s)
            else if s == Bottom then  
                normal (pi, r) 
            else OR (normal (pi, r) ) (normal (pi, s))
        
        Omega eff ->  
            case eff of 
                Bottom -> 
                    (pi, Bottom)
                Empty -> 
                    (pi, Empty)
                Omega inner -> 
                    normal (pi, Omega (inner))
                otherwise -> (pi, es)
        Dot r s -> 
            if r == Empty then 
                normal (pi,  s) 
            else if s == Empty then 
                normal (pi,  r) 
            else if r == Bottom then 
                (pi, Bottom)
            else (pi, es)
        Ttimes eff sv -> (pi, es)

                
        otherwise -> (pi, es)

{-


checkRedundent con = 
    let helper con1 con2 = 
            case con2 of 
                AndCon p1 p2 -> 
                    if con1 == p1 || con1 == p2 then True
                    else (helper con1 p1) && (helper con1 p2) 
                other -> if con1 == other then True 
                         else False
    in case con of 
        AndCon s1 s2 -> 
            if s1 == s2 then s1
            else if helper s1 s2 == True then checkRedundent s2
            else AndCon s1 (checkRedundent s2)
        otherwise -> con
        
         
fixed :: (Eq a) =>a -> a -> Bool
fixed a1 a2 =
    if a1 == a2 then True else False

normalCon :: Pure -> Pure
normalCon con =
    case con of 
        AndCon a b ->
            case (a, b) of 
                (FALSE, _) -> FALSE
                (_, FALSE) -> FALSE
                (TRUE, a) -> normalCon a
                (a, TRUE) -> normalCon a
                (Gt s1 n1, Gt s2 n2) -> 
                    if s1 /= s2 then con 
                    else 
                        if n1 > n2 then (Gt s1 n1) 
                        else (Gt s1 n2)
                (Gt s1 n1, Lt s2 n2) -> 
                    if s1 /= s2 then con 
                    else if n1 >= n2 then FALSE
                         else con 
                (Gt s1 n1, Eq s2 n2) -> 
                    if s1 /= s2 then con 
                    else if n1 >= n2 then FALSE
                         else (Eq s2 n2) 
                (Lt s1 n1, Gt s2 n2) -> 
                    if s1 /= s2 then con 
                    else if n2 >= n1 then FALSE
                         else con
                (Lt s1 n1, Lt s2 n2) -> 
                    if s1 /= s2 then con 
                    else if n2 > n1 then (Lt s1 n1) 
                         else (Lt s1 n2) 
                (Lt s1 n1, Eq s2 n2) -> 
                    if s1 /= s2 then con 
                    else if n2 >= n1 then FALSE
                         else (Eq s2 n2) 
                (Eq s1 n1, Gt s2 n2) -> 
                    if s1 /= s2 then con 
                    else if n2 >= n1 then FALSE
                         else (Eq s1 n1)
                (Eq s1 n1, Lt s2 n2) -> 
                    if s1 /= s2 then con 
                    else if n2 <= n1 then FALSE
                         else (Eq s1 n1)
                (Eq s1 n1, Eq s2 n2) -> 
                    if s1 /= s2 then con 
                    else if n2 /= n1 then FALSE
                         else (Eq s1 n1)
                (a, b) ->  
                    let na = normalCon a
                        nb = normalCon b
                        checkR = checkRedundent  (AndCon na nb)
                    in if fixed  checkR (normalCon checkR) then checkR
                       else normalCon checkR
                    
        otherwise -> con




inf :: ES -> Bool
inf effect = 
    case effect of 
        Bottom -> False
        Empty -> False
        Event _ -> False
        Dot e1 e2 -> inf e1 || inf e2
        OR e1 e2 -> inf e1 && inf e2
        Ttimes _ _ -> False
        Omega _ -> True

printEntailEFF :: ES -> ES -> String
printEntailEFF r s = 
    (printE r) ++ " |- " ++ (printE s)

computeSV :: Term -> Term
computeSV sv =
    case sv of 
        Add sv1 sv2 -> 
            case (sv1,sv2) of 
                (_, Value 0) -> sv1
                (Add (Iden id) (Value n), (Value n1) ) -> computeSV (Add (Iden id) (Value (n + n1)))
                (Minus (Iden id) (Value n), (Value n1) ) -> 
                    if n > n1 then computeSV (Minus (Iden id) (Value (n - n1)))
                    else computeSV (Add (Iden id) (Value (n1 - n)))
                (Value n1, Value n2) -> Value (n1 + n2)
                (Value n1, _) -> computeSV (Add sv2 sv1)
                otherwise -> if sv1 == sv2 then Mul sv1 (Value 2) else sv
        Minus sv1 sv2 -> 
            case (sv1,sv2) of 
                (Iden idl, Minus (Iden idr) (Value n1) ) -> 
                    if idl == idr then (Value n1)
                    else Minus sv1 sv2
                (ddd, Value 0) -> ddd
                (Value n1, Value n2) -> Value (n1 - n2)
                (Minus sv1 (Value n1), Value n2) -> computeSV (Minus sv1 (Value (n1 +n2)))
                (Value n1, _) -> Minus sv2 sv1
                otherwise -> if sv1 == sv2 then Value 0 else sv
        Div sv1 sv2 -> 
            case (sv1,sv2) of 
                (Value n1, Value n2) -> Value (n1 `div` n2)
                otherwise -> sv
        Mul sv1 sv2 -> 
            case (sv1,sv2) of 
                (Value n1, Value n2) -> Value (n1 * n2)
                (Value n1, _) -> Mul sv2 sv1
                otherwise -> sv
        otherwise -> sv

 



derivatives :: Effect -> ES -> ES
derivatives (cond, effect) head = 
    case normal effect of 
        Bottom -> Bottom
        Empty -> 
            if (head) ==  Empty then Empty 
            else Bottom
        Event a -> 
            if (head) ==  Event a then Empty 
            else Bottom
        Dot e1 e2 -> normal (Dot (derivatives (cond, e1) head) e2)
        OR e1 e2 -> normal (OR (derivatives (cond, e1) head) (derivatives (cond, e2) head))
        Omega e -> normal (Dot (derivatives (cond, e) head) effect )
        Ttimes eff sv -> 
            case head of 
                Ttimes effh svh -> normal (Ttimes eff (Minus sv svh))
                otherwise -> Bottom

            
unifyCondition :: Effect -> ES
unifyCondition (Pure , effect) =
    let rewriteSV str num sv = 
            case sv of 
                Iden id -> if id == str then Value num else  sv  
                Value n -> sv
                Add sv1 sv2 -> computeSV ( Add (rewriteSV str num sv1) (rewriteSV str num sv2))
                Minus sv1 sv2 -> computeSV ( Minus (rewriteSV str num sv1) (rewriteSV str num sv2))
                Div sv1 sv2 -> computeSV ( Div (rewriteSV str num sv1) (rewriteSV str num sv2))
                Mul sv1 sv2 -> computeSV ( Mul (rewriteSV str num sv1) (rewriteSV str num sv2))
        rewriteEff :: String -> Int -> ES -> ES
        rewriteEff str num eff = 
            case eff of
                Ttimes effin sv -> normal (Ttimes effin (rewriteSV str num sv))
                Dot e1 e2 -> Dot (rewriteEff str num  e1) (rewriteEff  str num e2)
                OR e1 e2 -> OR (rewriteEff str num  e1) (rewriteEff str num  e2)
                otherwise ->eff
        rewriteEq :: Pure -> ES -> ES
        rewriteEq con eff =
            case con of 
                Eq str value -> rewriteEff str value eff
                AndCon c1 c2 -> rewriteEq c1 (rewriteEq c2 eff)
                otherwise -> eff
    in  (rewriteEq Pure effect)

head_is_Time :: Effect -> Effect -> ES-> Env -> [(Tree String ,Bool)]
head_is_Time (cfL) cfR head env = 
    let (condL, effL) = cfL
        (condR, effR) = cfR
        Ttimes eff sv = head
    in case sv of 
        Iden id -> 
            let cond0 = Eq id 0
                cond0L = checkRedundent $ normalCon (AndCon cond0 condL)
                cond0R = checkRedundent $ normalCon (AndCon cond0 condL)
                unified0L = unifyCondition (cond0L, effL)
                unified0R = unifyCondition (cond0R, effR)
                condS = Gt id 0
                condSL = normalCon (AndCon condS condL)
                condSR = normalCon (AndCon condS condR)
                
            in [entailConditionalEff (condSL, (derivatives cfL head))(condSR, (derivatives cfR head)) env]
               ++ [entailConditionalEff (cond0L, normal unified0L)(cond0R, normal unified0R) env] 
        Minus (Iden id) (Value n) ->
            let cond0 = Eq id n
                cond0L = checkRedundent $ normalCon (AndCon cond0 condL)
                cond0R = checkRedundent $ normalCon (AndCon cond0 condL)
                unified0L = unifyCondition (cond0L, effL)
                unified0R = unifyCondition (cond0R, effR)
                condS = Gt id n
                condSL = normalCon (AndCon condS condL)
                condSR = normalCon (AndCon condS condR)
                
            in [entailConditionalEff (condSL, (derivatives cfL head))(condSR, (derivatives cfR head)) env]
               ++ [entailConditionalEff (cond0L, normal unified0L)(cond0R, normal unified0R) env] 
        
unfold :: Effect -> Effect -> Env -> (Tree String ,Bool)
unfold cfL cfR env= 
    let (condL, effL) = cfL
        (condR, effR) = cfR
        heads = first cfL
        timesHeads = filter (\h -> case h of 
            Ttimes _ _ -> True 
            otherwise -> False) heads
        nonTimesHeads = filter (\h -> case h of 
            Ttimes _ _ ->  False
            otherwise -> True) heads
        resultNonTimesL = map (\h -> entailConditionalEff 
                (condL, (derivatives cfL h))
                (condR, (derivatives cfR h))
                env) nonTimesHeads
        resultTimesL = foldr (\h acc -> acc ++ (head_is_Time cfL cfR h env)  ) []  timesHeads
        (trees, result) = foldr (\(tree, re) (accT, accR)-> (accT++[tree], re || accR) ) ([], False) (resultNonTimesL++ resultTimesL)
    in (Node (printEntailCondEff (condL, (normal effL)) (condR, (normal effR))) trees, result)

 

entailConditionalEff:: Effect -> Effect -> Env -> (Tree String ,Bool)
entailConditionalEff cfL cfR env=
    let (condL, effL) = cfL
        (condR, effR) = cfR
    in if (inf effL) == False && (inf effR) == True then
            (Node ((printEntailCondEff cfL cfR) ++ " [Disprove-Inf]") [], False) 
       else if effL == Empty && effR /= Empty  then
            (Node ((printEntailCondEff cfL cfR) ++ " [Disprove-Emp]") [], False) 
       --else if  effL == Empty && effR == Empty  then (Node ((printEntailCondEff cfL cfR) ) [], True)
       else if  effL /= Empty && effR == Empty  then (Node ((printEntailCondEff cfL cfR) ++ " [Residue: " ++ printE effL ++"]" ) [], True)
       else if (cfL,cfR) `elem` env then (Node ((printEntailCondEff cfL cfR ) ++" [In context!]" ) [], True)
       else let env' = env ++ [(cfL, cfR)]
            in unfold cfL cfR env'
           

report :: Effect -> Effect  -> IO()
report cfL cfR = 
    let (tree, result) =  entailConditionalEff cfL cfR []
    in case result of 
        True -> 
            do {
                putStrLn ("============= Report =============");
                putStrLn ("GOAL: " ++ printEntailCondEff cfL cfR);
                putStrLn "Succeed!" ;
                putStrLn $ drawTree tree
            }
        False -> 
            do {
                putStrLn ("============= Report =============");
                putStrLn ("GOAL: " ++ printEntailCondEff cfL cfR);
                putStrLn ("Failed" ) ;
                putStrLn $ drawTree tree
            }

-}