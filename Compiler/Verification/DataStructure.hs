module Verification.DataStructure where

data Condition 
    = TRUE
    | FALSE
    | Gt String Int
    | Lt String Int
    | Eq String Int
    | AndCon Condition Condition
    deriving (Show, Eq)

data Singleton a = Event a | NegEv a deriving (Show, Eq)

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
    | Star Effect 
    | Omega Effect 
    -- | And Effect Effect 
    -- | Neg Effect  
    
    deriving (Show, Eq)

type Env =  [(Effect, Effect)] 
type ConditionalEff = (Condition, Effect)


-- to print symbolic values 
printSV ::SymbolicValue -> String
printSV sv = 
    case sv of 
        Iden str -> str
        Value num -> show num
        Add sv1 sv2 -> "(" ++ (printSV sv1) ++ "+"++(printSV sv2) ++")"
        Minus sv1 sv2 -> "(" ++ (printSV sv1) ++ "-"++(printSV sv2) ++")"
        Div sv1 sv2 -> "(" ++ (printSV sv1) ++ "/"++(printSV sv2) ++")"
        Mul sv1 sv2 -> "(" ++ (printSV sv1) ++ "*"++(printSV sv2) ++")"

-- to print effects 
printE :: Effect -> String
printE effect =
    case effect of 
        Bottom -> "_"
        Empty -> "emp"
        Singleton str -> str
        Dot eff1 eff2 -> "(" ++ (printE eff1) ++ ". "++(printE eff2) ++")"
        OR eff1 eff2 -> "(" ++ (printE eff1) ++ " + "++(printE eff2) ++")"
        Star eff -> (printE eff) ++ "^*"
        Ttimes eff sv -> (printE eff) ++ "^" ++ (printSV sv)
        Omega eff -> (printE eff) ++ "^w"
        -- And eff1 eff2 -> "(" ++ (printE eff1) ++ " & "++(printE eff2) ++")"
        -- Neg eff  -> "!" ++ (printE eff) 

-- to get the evnt from Singleton effects 
printS :: Singleton String -> String 
printS (Event str) =  show str
printS (NegEv str) =  show str

-- print entailment of two effects
printEntail :: Effect -> Effect -> String
printEntail r s = 
    (printE r) ++ " |- " ++ (printE s)

-- print conditions 
printCon :: Condition -> String
printCon con =
    case con of 
        TRUE -> "(true)"
        FALSE -> "(false)"
        Gt str num -> "(" ++ str ++ ">" ++ show num ++ ")"
        Lt str num -> "(" ++str ++ "<" ++ show num ++ ")"
        Eq str num -> "(" ++str ++ "=" ++ show num ++ ")"
        AndCon con1 con2 -> "(" ++(printCon con1) ++ "/\\" ++  (printCon con2) ++ ")"
        
printCondEff :: ConditionalEff ->String
printCondEff conEff =
    case conEff of 
        -- (FALSE, _) -> ""
        (con,eff) -> printCon con ++ "/\\" ++ printE eff

printEntail1 :: Condition -> Effect-> Effect ->String
printEntail1 con eff1 eff2 =
    "[Condition: "++ printCon con ++ "] " ++ printEntail eff1 eff2

printEntailCondEff :: ConditionalEff -> ConditionalEff -> String
printEntailCondEff cf1 cf2 = 
    (printCondEff cf1) ++ " |- " ++ (printCondEff cf2)