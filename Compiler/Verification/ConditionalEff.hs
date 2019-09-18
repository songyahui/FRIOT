module Verification.ConditionalEff where
import Debug.Trace
import Data.Tree
import Verification.Antimirov
import Data.List

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
        -- (FALSE, _) -> ""
        (con,eff) -> printCon con ++ "/\\" ++ printE eff

printEntail1 :: Condition -> Effect-> Effect ->String
printEntail1 con eff1 eff2 =
    "[Condition: "++ printCon con ++ "] " ++ printEntail eff1 eff2

printEntailCondEff :: ConditionalEff -> ConditionalEff -> String
printEntailCondEff cf1 cf2 = 
    (printCondEff cf1) ++ " |- " ++ (printCondEff cf2)


dotHead :: Effect -> [Effect]  -- a^t.b -> a^t
dotHead eff =
    case eff of 
        Bottom -> []
        Empty -> [Empty]
        Singleton ev -> [eff]
        Star r -> [eff]
        Omega r -> [eff]
        Ttimes eff sv -> [eff]
        Dot h rest -> dotHead h
        And left right -> intersect (dotHead left) (dotHead right)
        OR e1 e2 -> union (dotHead e1) (dotHead e2) 
        Neg e -> [eff]


splitDot :: Effect -> [(Effect, Effect)]  -- a^t.b -> (a^t, b)
splitDot eff = 
    case eff of 
        Bottom -> []
        Empty -> [(Empty, Empty)]
        Singleton ev -> [(eff, Empty)]
        Star r -> [(eff, Empty)]
        Omega r -> [(eff, Empty)]
        Ttimes e sv -> [(eff, Empty)]
        Dot h rest -> 
            let pairs = splitDot h
                heads = map (\(e1,e2) -> e1) pairs
                rests = map (\(e1,e2) -> normal (Dot e2 rest)) pairs
            in zip heads rests
        And left right -> intersect (splitDot left) (splitDot right)
        OR e1 e2 ->  (splitDot e1)++ (splitDot e2) 
        Neg e -> [(eff, Empty)]

normalCon :: Condition -> Condition
normalCon con =
    case con of 
        AndCon a b ->
            case (a, b) of 
                (FALSE, _) -> TRUE
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


t_t con evn (h1, rest1) (h2,rest2) = 
    case (h1, h2) of 
        (Ttimes e1 sv1, Ttimes e2 sv2) ->
            let (tree, result) = (containment e1 e2 [])
            in 
                if result ==  False then (tree, result)
                else 
                    let afterL = rest1
                        afterS = normal ((Dot (normal (Ttimes e2 (Minus sv2 sv1 ))) rest2))
                    in entail (con, normal afterL) (con, normal afterS) evn


checkFirstElm :: Condition -> Effect -> Effect -> Env -> (Tree String ,Bool)
checkFirstElm con eff1 eff2 evn = 
    let ceff1SplitL = splitDot eff1
        ceff2SplitL = splitDot eff2
        cartProd = [(x,y) | x <- ceff1SplitL, y <- ceff2SplitL]
        checkHead (h1, rest1) (h2,rest2) =
            case (h1, h2) of
                (Ttimes e1 sv1, Ttimes e2 sv2) -> 
                    t_t con evn (h1, rest1) (h2,rest2)
                (Ttimes e1 sv1, Star e2) ->
                    let (tree, result) = (containment e1 e2 [])
                    in 
                        if result ==  False then (tree, result)
                        else 
                            let afterL = rest1
                                afterS = normal (Dot h2 rest2)
                            in entail (con, normal afterL) (con, normal afterS) evn

                
                (Ttimes e1 sv1, Omega e2) ->
                    let (tree, result) = (containment e1 e2 [])
                    in 
                        if result ==  False then (tree, result)
                        else 
                            let afterL = rest1
                                afterS = normal (Dot h2 rest2)
                            in entail (con, normal afterL) (con, normal afterS) evn

                    {-
                (Ttimes e1 sv1, eff) -> -- others
        
                (Star e1, Ttimes e2 sv2) ->
                (Star e1, Star e2) ->
                (Star e1, Omega e2) ->
                (Star e1, eff) -> -- others

                (Omega e1, Ttimes e2 sv2) ->
                (Omega e1, Star e2) ->
                (Omega e1, Omega e2) ->
                (Omega e1, eff) -> -- others

                (eff, Ttimes e2 sv2) ->
                (eff, Star e2) ->
                (eff, Omega e2) ->
                (eff, eff) -> -- others
                -}
                otherwise -> containment eff1 eff2 []
            
            
        helper (l, s) acc =
            let (nodeNow, resultNow ) = acc
                (node, result) =  checkHead l s
            in case nodeNow of 
                Node str list -> ( Node str (list ++ [node]) , resultNow && result)
            
        
    in foldr helper (Node (printEntail1 con eff1 eff2) [] ,True) cartProd
        
    
entail :: ConditionalEff -> ConditionalEff -> Env -> (Tree String ,Bool)
entail ceff1 ceff2 evn = 
    let (con1, eff1) = ceff1
        (con2, eff2) = ceff2
        andCondition = normalCon (AndCon con1 con2)
    in 
        if andCondition == FALSE 
            then (Node ((printEntailCondEff ceff1 ceff2 )++ " [Condition Contradictory]") [] ,False)
        else 
            checkFirstElm andCondition (normal eff1) (normal eff2) evn


extentCondition :: ConditionalEff -> [ConditionalEff]
extentCondition (condition, effect) =
    let helper (frame, con) eff = 
            case eff of 
                Dot eff1 eff2 ->  
                    let combine = [(x,y) | x <- helper (frame, con) eff1, y <- helper (frame, con) eff2] 
                        merge ((conh ,effh),(conr, effr)) =
                            (AndCon conh conr, normal (Dot frame (normal (Dot effh effr))))
                    in map merge combine 
                OR eff1 eff2 ->  
                    let combine = [(x,y) | x <- helper (frame, con) eff1, y <- helper (frame, con) eff2] 
                        merge ((conh ,effh),(conr, effr)) =
                            (AndCon conh conr, normal (Dot frame (normal (OR effh effr))))
                    in map merge combine 
                Ttimes ins (Iden s) -> [(Eq s 0, Empty) , (Gt s 0, Ttimes ins (Iden s))]

                otherwise -> [(con, normal (Dot frame eff))]
    in helper (Empty , condition)  effect

p_Con_R :: ConditionalEff -> ConditionalEff  -> IO ()
p_Con_R r s  = 
    let (tree , res) = entail r s []
    in
     if res then 
        do {
        putStrLn ("============= Report =============");
        putStrLn ("GOAL: " ++ printEntailCondEff r s);
        print "Succeed!" ;
        putStrLn $ drawTree tree
        }
     
     else 
        do {
        putStrLn ("============= Report =============");
        putStrLn ("GOAL: " ++ printEntailCondEff r s);
        print "fail";
        putStrLn $ drawTree tree
        }
        {-
        case getResidue r s [] of 
            Nothing -> 
                do {
                    putStrLn ("============= Report =============");
                    putStrLn ("GOAL: " ++ printEntailCondEff r s);
                    print "Failed.";
                    putStrLn $ drawTree tree
                }
            Just eff -> 
                do {
                    putStrLn ("============= Report =============");
                    putStrLn ("GOAL: " ++ printEntailCondEff r s);
                    print ("Succeed with a residue: "  ++ printE eff);
                    putStrLn $ drawTree tree
                }

    -}
