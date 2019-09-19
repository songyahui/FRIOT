module Verification.ConditionalEff where
import Debug.Trace
import Data.Tree
import Verification.DataStructure
import Verification.Antimirov
import Verification.Helpers
import Data.List


getCon :: ConditionalEff -> Condition
getCon (con ,eff) = con

getEff :: ConditionalEff -> Effect
getEff (con ,eff) = eff
     
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
        -- And left right -> intersect (dotHead left) (dotHead right)
        OR e1 e2 -> union (dotHead e1) (dotHead e2) 
        -- Neg e -> [eff]


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
        -- And left right -> intersect (splitDot left) (splitDot right)
        OR e1 e2 ->  (splitDot e1)++ (splitDot e2) 
        -- Neg e -> [(eff, Empty)]

checkRedundent :: Condition -> Condition
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



checkFirstElm :: Condition -> Effect -> Effect -> Env -> (Tree String ,Bool)
checkFirstElm con eff1 eff2 evn = 
    let ceff1SplitL = splitDot eff1
        ceff2SplitL = splitDot eff2
        cartProd = [(x,y) | x <- ceff1SplitL, y <- ceff2SplitL]
        checkHead (h1, rest1) (h2,rest2) =
            case (h1, h2) of
                (Ttimes e1 sv1, Ttimes e2 sv2) -> 
                    let (tree, result) = (containment e1 e2 [])
                    in 
                        if result ==  False then (tree, result)
                        else 
                            let afterL = rest1
                                afterS = normal ((Dot (normal (Ttimes e2 (Minus sv2 sv1 ))) rest2))
                            in entail (con, normal afterL) (con, normal afterS) evn
                
                            
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
        
    

{-
entail -> used to entail two effects with their condition
    1. conjunct their condition, and check redundent
    2. unify the conjuncted condition with LHS and RHS
-}
entail :: ConditionalEff -> ConditionalEff -> Env -> (Tree String ,Bool)
entail ceff1 ceff2 evn = 
    let (con1, r) = ceff1
        (con2, s) = ceff2
        andCondition = checkRedundent $ normalCon (AndCon con1 con2)
        unifiedR = unifyCondition (andCondition, r)
        unifiedS = unifyCondition (andCondition, s)
    in 
        if andCondition == FALSE 
            then (Node ((printEntailCondEff ceff1 ceff2 )++ " [Condition Contradictory]") [] ,False)
        else 
            checkFirstElm andCondition (normal unifiedR) (normal unifiedS) evn


{-
extentCondition -> used to capture the branching of a effect if it contains any Ttimes part. 
    the very first step of checking effects, 
    given an effect, to get all the conditions from the Ttimes effect.
-}
extentCondition :: ConditionalEff -> [ConditionalEff]
extentCondition (condition, effect) =
    let helper (frame, con) eff = 
            case eff of 
                Dot eff1 eff2 ->  
                    let combine = [(x,y) | x <- helper (frame, con) eff1, y <- helper (frame, con) eff2] 
                        merge ((conh ,effh),(conr, effr)) =
                            (checkRedundent $ normalCon (AndCon  (AndCon con conh) (AndCon con conr)), normal (Dot frame (normal (Dot effh effr))))
                    in map merge combine 
                OR eff1 eff2 ->  
                    let combine = [(x,y) | x <- helper (frame, con) eff1, y <- helper (frame, con) eff2] 
                        merge ((conh ,effh),(conr, effr)) =
                            (checkRedundent $ normalCon (AndCon  (AndCon con conh) (AndCon con conr)), normal (Dot frame (normal (OR effh effr))))
                    in map merge combine 
                Ttimes ins (Iden s) -> [(Eq s 0, Empty) , (Gt s 0, eff)]
                Ttimes ins (Minus (Iden s) (Value n)) -> [(Eq s n, Empty) , (Gt s n, eff)]
                otherwise -> [(con, normal (Dot frame eff))]
        filterNotFalse (condition, effect) = 
            case condition of
                FALSE -> False
                otherwise -> True
    in filter filterNotFalse (helper (Empty , condition)  effect)

{-
unifyCondition ->  to make the effect consistant with the conditions upon the equality.
-}
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

p_Con_R :: ConditionalEff -> ConditionalEff  -> IO ()
p_Con_R r s  = 
    let 
        conRL = extentCondition r
        conSL = extentCondition s
        combine = [(x,y) | x <- conRL, y <- conSL] 
        
        printReprot [] = print ("END.") 
        printReprot (l:ls)=  
            let (r, s) = l
                (tree , res) = entail r s []
            in 
                if res then 
                    do {
                    putStrLn ("============= Report =============");
                    putStrLn ("GOAL: " ++ printEntailCondEff r s);
                    print "Succeed!" ;
                    putStrLn $ drawTree tree;
                    printReprot ls
                    }
                 else 
                    do{
                    putStrLn ("============= Report =============");
                    putStrLn ("GOAL: " ++ printEntailCondEff r s);
                    print "fail";
                    putStrLn $ drawTree tree;
                    printReprot ls
                    }

    in  printReprot combine
     
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
