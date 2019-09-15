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


getFirstDot :: Effect -> [Effect]
getFirstDot eff =
    case eff of 
        Dot h rest -> [h]
        And left right -> (getFirstDot left) ++ (getFirstDot right)
        otherwise -> [eff] -- 

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
                    else if n1 > n2 then FALSE
                         else con 
                (Gt s1 n1, Eq s2 n2) -> 
                    if s1 /= s2 then con 
                    else if n1 > n2 then FALSE
                         else (Eq s2 n2) 
                (Lt s1 n1, Gt s2 n2) -> 
                    if s1 /= s2 then con 
                    else if n2 > n1 then FALSE
                         else con
                (Lt s1 n1, Lt s2 n2) -> 
                    if s1 /= s2 then con 
                    else if n2 > n1 then (Lt s1 n1) 
                         else (Lt s1 n2) 
                (Lt s1 n1, Eq s2 n2) -> 
                    if s1 /= s2 then con 
                    else if n2 > n1 then FALSE
                         else (Eq s2 n2) 
                (Eq s1 n1, Gt s2 n2) -> 
                    if s1 /= s2 then con 
                    else if n2 > n1 then FALSE
                         else (Eq s1 n1)
                (Eq s1 n1, Lt s2 n2) -> 
                    if s1 /= s2 then con 
                    else if n2 < n1 then FALSE
                         else (Eq s1 n1)
                (Eq s1 n1, Eq s2 n2) -> 
                    if s1 /= s2 then con 
                    else if n2 /= n1 then FALSE
                         else (Eq s1 n1)
                otherwise -> normalCon (AndCon (normalCon a ) (normalCon b))
        otherwise -> con


{-entail :: ConditionalEff -> ConditionalEff -> Env -> (Tree String ,Bool)
entail (con1, eff1) (con2, eff2) evn = 
    let firstDot1 = getFirstCongetFirstDot eff1
        firstDot2 = getFirstCongetFirstDot eff2
        

    case (eff1) (eff2) of
        (Ttimes e1 sv1, Ttimes e2 sv2) ->
            if e1 == e2 then 
            containment eff1 eff2
        otherwise -> containment eff1 eff2
    -- trace ( (printEntailCondEff cf1 cf2))
    -- (Node (printCondEff cf1) [], False)
    -}

