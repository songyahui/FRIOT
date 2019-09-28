module Verification.Rewriting where
import Data.Tree

type ErrorMsg = String

data Result a = 
    OK (Tree String)
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


printEntailCondEff :: ConditionalEff -> ConditionalEff -> String
printEntailCondEff cf1 cf2 = 
    (printCondEff cf1) ++ " |- " ++ (printCondEff cf2)

type ConditionalEff = (Condition, Effect)
type Env =  [(ConditionalEff, ConditionalEff)] 


nullable :: ConditionalEff -> Bool
nullable (cond, effect) = 
    case effect of 
        Bottom -> False
        Empty -> True
        Singleton _ -> False
        Dot e1 e2 -> nullable (cond, e1) && nullable (cond, e2)
        OR e1 e2 -> nullable (cond, e1) || nullable (cond, e2)
        Omega _ -> False
        Ttimes eff sv -> 
            case (cond, sv) of 
                (Eq str1 num, Iden str2) -> 
                    if str1 == str2 then False 
                    else True
                (AndCon con1 con2, _) -> nullable (con1, effect) && nullable (con2, effect)
                otherwise -> True

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
                Value 0 -> Empty
                otherwise -> effect
                
        otherwise -> effect
        

entailConditionalEff:: ConditionalEff -> ConditionalEff -> Env -> Result ErrorMsg
entailConditionalEff cfL cfR env=
    let (condL, effL) = cfL
        (condR, effR) = cfR
    in if (inf effL) == False && (inf effR) == True then
            Error (Node ((printEntailEFF (normal effL) (normal effL) )) []) "[Disprove2!!!]"
       else if (nullable cfL) == True && (nullable cfR) == False then
            Error (Node ((printEntailEFF (normal effL) (normal effL) )) []) "[Disprove1!!!]"
       else if (cfL,cfR) `elem` env then OK (Node "test" [])
       else OK (Node "test" [])


report :: ConditionalEff -> ConditionalEff -> Env  -> IO()
report cfL cfR env = 
    let result =  entailConditionalEff cfL cfR env
    in case result of 
        OK tree -> 
            do {
                putStrLn ("============= Report =============");
                putStrLn ("GOAL: " ++ printEntailCondEff cfL cfR);
                putStrLn "Succeed!" ;
                putStrLn $ drawTree tree
            }
        Error tree msg -> 
            do {
                putStrLn ("============= Report =============");
                putStrLn ("GOAL: " ++ printEntailCondEff cfL cfR);
                putStrLn ("Failed: " ++ msg) ;
                putStrLn $ drawTree tree
            }