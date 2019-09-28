module Verification.Rewriting where
import Data.Tree
import Data.List

type ErrorMsg = String

data Result a = 
    OK (Tree String)
    | Residue (Tree String) Effect
    | Error (Tree String) a
    deriving (Show, Eq)

data Condition 
    = TRUE
    | FALSE
    | Gt String Int
    | Lt String Int
    | Eq String Int
    | AndCon Condition Condition
    deriving (Show, Eq)

data SymbolicValue 
    = Iden String
    | Value Int
    | Add SymbolicValue SymbolicValue
    | Minus SymbolicValue SymbolicValue
    | Div SymbolicValue SymbolicValue
    | Mul SymbolicValue SymbolicValue
    deriving (Show, Eq)

data Effect 
    = Bottom 
    | Empty
    | Singleton String
    | Dot Effect Effect
    | OR Effect Effect 
    | Ttimes (Effect) SymbolicValue -- assuming T cannot be negetive
    | Omega Effect 
    deriving (Show, Eq)

printE :: Effect -> String
printE effect =
    case effect of 
        Bottom -> "_"
        Empty -> "emp"
        Singleton str -> str
        Dot eff1 eff2 -> "(" ++ (printE eff1) ++ ". "++(printE eff2) ++")"
        OR eff1 eff2 -> "(" ++ (printE eff1) ++ " + "++(printE eff2) ++")"
        Ttimes eff sv -> (printE eff) ++ "^" ++ (printSV sv)
        Omega eff -> (printE eff) ++ "^w"

printCon :: Condition -> String
printCon con =
    case con of 
        TRUE -> "(true)"
        FALSE -> "(false)"
        Gt str num -> "(" ++ str ++ ">" ++ show num ++ ")"
        Lt str num -> "(" ++str ++ "<" ++ show num ++ ")"
        Eq str num -> "(" ++str ++ "=" ++ show num ++ ")"
        AndCon con1 con2 -> "(" ++(printCon con1) ++ "/\\" ++  (printCon con2) ++ ")"

printSV ::SymbolicValue -> String
printSV sv = 
    case sv of 
        Iden str -> str
        Value num -> show num
        Add sv1 sv2 -> "(" ++ (printSV sv1) ++ "+"++(printSV sv2) ++")"
        Minus sv1 sv2 -> "(" ++ (printSV sv1) ++ "-"++(printSV sv2) ++")"
        Div sv1 sv2 -> "(" ++ (printSV sv1) ++ "/"++(printSV sv2) ++")"
        Mul sv1 sv2 -> "(" ++ (printSV sv1) ++ "*"++(printSV sv2) ++")"


printCondEff :: ConditionalEff ->String
printCondEff conEff =
    case conEff of 
        -- (FALSE, _) -> ""
        (con,eff) -> printCon con ++ "/\\" ++ printE eff


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

normalCon :: Condition -> Condition
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

printEntailCondEff :: ConditionalEff -> ConditionalEff -> String
printEntailCondEff cf1 cf2 = 
    (printCondEff cf1) ++ " |- " ++ (printCondEff cf2)

type ConditionalEff = (Condition, Effect)
type Env =  [(ConditionalEff, ConditionalEff)] 


inf :: Effect -> Bool
inf effect = 
    case effect of 
        Bottom -> False
        Empty -> False
        Singleton _ -> False
        Dot e1 e2 -> inf e1 || inf e2
        OR e1 e2 -> inf e1 && inf e2
        Ttimes _ _ -> False
        Omega _ -> True

printEntailEFF :: Effect -> Effect -> String
printEntailEFF r s = 
    (printE r) ++ " |- " ++ (printE s)

computeSV :: SymbolicValue -> SymbolicValue
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

 

normal :: Effect -> Effect
normal effect =
     case effect of
        OR r s -> 
            if r == s then 
                normal r
            else if r == Bottom then  
                normal s
            else if s == Bottom then  
                normal r 
            else OR (normal r) (normal s)
        
        Omega eff ->  
            case eff of 
                Bottom -> 
                    Bottom
                Empty -> 
                    Empty
                Omega inner -> 
                    Omega (inner)
                otherwise -> effect
        Dot r s -> 
            if r == Empty then 
                normal s 
            else if s == Empty then 
                normal r 
            else if r == Bottom then 
                Bottom
            else 
                case (normal r, normal s) of
                    (Ttimes inr svt, Ttimes ins svs) -> 
                        if (inr) == (ins) then (Ttimes inr (computeSV (Add svt svs ))) else Dot (normal r) (normal s)
                    (_, Ttimes ins svs) -> 
                        if r == ins then normal (Ttimes ins (computeSV (Add svs (Value 1) ))) else Dot (normal r) (normal s)
                    (a,b) -> (Dot a b)
        Ttimes eff sv -> 
            case computeSV sv of  
                Value num -> 
                    let helper 0 = Empty
                        helper n = Dot eff (helper (n-1))
                    in helper num
                otherwise -> effect
                
        otherwise -> effect

derivatives :: ConditionalEff -> Effect -> Effect
derivatives (cond, effect) head = 
    case normal effect of 
        Bottom -> Bottom
        Empty -> 
            if (head) ==  Empty then Empty 
            else Bottom
        Singleton a -> 
            if (head) ==  Singleton a then Empty 
            else Bottom
        Dot e1 e2 -> normal (Dot (derivatives (cond, e1) head) e2)
        OR e1 e2 -> normal (OR (derivatives (cond, e1) head) (derivatives (cond, e2) head))
        Omega e -> normal (Dot (derivatives (cond, e) head) effect )
        Ttimes eff sv -> 
            case head of 
                Ttimes effh svh -> normal (Ttimes eff (Minus sv svh))
                otherwise -> Bottom

            
unifyCondition :: ConditionalEff -> Effect
unifyCondition (condition , effect) =
    let rewriteSV str num sv = 
            case sv of 
                Iden id -> if id == str then Value num else  sv  
                Value n -> sv
                Add sv1 sv2 -> computeSV ( Add (rewriteSV str num sv1) (rewriteSV str num sv2))
                Minus sv1 sv2 -> computeSV ( Minus (rewriteSV str num sv1) (rewriteSV str num sv2))
                Div sv1 sv2 -> computeSV ( Div (rewriteSV str num sv1) (rewriteSV str num sv2))
                Mul sv1 sv2 -> computeSV ( Mul (rewriteSV str num sv1) (rewriteSV str num sv2))
        rewriteEff :: String -> Int -> Effect -> Effect
        rewriteEff str num eff = 
            case eff of
                Ttimes effin sv -> normal (Ttimes effin (rewriteSV str num sv))
                Dot e1 e2 -> Dot (rewriteEff str num  e1) (rewriteEff  str num e2)
                OR e1 e2 -> OR (rewriteEff str num  e1) (rewriteEff str num  e2)
                otherwise ->eff
        rewriteEq :: Condition -> Effect -> Effect
        rewriteEq con eff =
            case con of 
                Eq str value -> rewriteEff str value eff
                AndCon c1 c2 -> rewriteEq c1 (rewriteEq c2 eff)
                otherwise -> eff
    in  (rewriteEq condition effect)

head_is_Time :: ConditionalEff -> ConditionalEff -> Effect-> Env -> [(Tree String ,Bool)]
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
        
unfold :: ConditionalEff -> ConditionalEff -> Env -> (Tree String ,Bool)
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

first :: ConditionalEff -> [Effect]
first (cond, effect) = 
    case effect of 
        Bottom -> []
        Empty -> [Empty]
        Singleton ev -> [effect]
        Dot e1 e2 -> first (cond, e1)
        OR e1 e2 -> union (first (cond, e1)) (first (cond, e2))  
        Omega r -> first (cond, r)
        Ttimes eff sv -> [Ttimes eff sv] 

entailConditionalEff:: ConditionalEff -> ConditionalEff -> Env -> (Tree String ,Bool)
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
           

report :: ConditionalEff -> ConditionalEff  -> IO()
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