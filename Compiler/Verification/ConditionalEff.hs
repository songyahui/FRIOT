module Verification.ConditionalEff where
import Debug.Trace
import Data.Tree
import Verification.Antimirov


data Condition 
    = TRUE
    | FALSE
    | Gt String Int
    | Lt String Int
    | Eq String Int
    | AndCon Condition Condition
    deriving (Show, Eq)

printCon :: Condition -> String
printCon con =
    case con of 
        TRUE -> "(true)"
        FALSE -> "(false)"
        Gt str num -> "(" ++ str ++ ">" ++ show num ++ ")"
        Lt str num -> "(" ++str ++ "<" ++ show num ++ ")"
        Eq str num -> "(" ++str ++ "=" ++ show num ++ ")"
        AndCon con1 con2 -> "(" ++(printCon con1) ++ "/\\" ++  (printCon con2) ++ ")"
type ConditionalEff = (Condition, Effect)
     
printConditionalEff :: ConditionalEff ->String
printConditionalEff conEff =
    case conEff of 
        (con,eff) -> printCon con ++ printE eff