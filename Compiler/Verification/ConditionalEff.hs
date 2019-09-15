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

getCon :: ConditionalEff -> Condition
getCon (con ,eff) = con

getEff :: ConditionalEff -> Effect
getEff (con ,eff) = eff
     
printCondEff :: ConditionalEff ->String
printCondEff conEff =
    case conEff of 
        (con,eff) -> printCon con ++ "/\\" ++ printE eff

printEntailCondEff :: ConditionalEff -> ConditionalEff -> String
printEntailCondEff cf1 cf2 = 
    (printCondEff cf1) ++ " |- " ++ (printCondEff cf2)



entail :: ConditionalEff -> ConditionalEff -> Env -> (Tree String ,Bool)
entail cf1 cf2 evn = 
    case (getEff cf1) (getEff cf2) of
        ()
    -- trace ( (printEntailCondEff cf1 cf2))
    -- (Node (printCondEff cf1) [], False)

