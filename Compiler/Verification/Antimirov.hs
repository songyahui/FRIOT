module Verification.Antimirov where
import Debug.Trace
import Data.Tree

subtree :: [Tree (String, String)]
subtree = [ Node ("head", "foo") []
            , Node ("head","bars") [ Node ("head","oi!") []
            , Node ("head","baz") [ Node ("head","a") [ Node ("head","b") []
                                      , Node ("head","c") []]
                           , Node ("head","d") [ Node ("head","e") []]]]
            , Node ("head","foobar") []]


tree :: Tree (String, String)
tree = Node ("head","hello") subtree

-- putStrLn $ drawTree tree

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
    | And Effect Effect 
    | Star Effect 
    | Neg Effect  
    | Ttimes Effect SymbolicValue -- assuming T cannot be negetive
    | Omega Effect 
    deriving (Show, Eq)

type Env =  [(Effect, Effect)] 

printSV ::SymbolicValue -> String
printSV sv = 
    case sv of 
        Iden str -> str
        Value num -> show num
        Add sv1 sv2 -> "(" ++ (printSV sv1) ++ "+"++(printSV sv2) ++")"
        Minus sv1 sv2 -> "(" ++ (printSV sv1) ++ "-"++(printSV sv2) ++")"
        Div sv1 sv2 -> "(" ++ (printSV sv1) ++ "/"++(printSV sv2) ++")"
        Mul sv1 sv2 -> "(" ++ (printSV sv1) ++ "*"++(printSV sv2) ++")"

computeSV :: SymbolicValue -> SymbolicValue
computeSV sv =
    case sv of 
        Add sv1 sv2 -> 
            case (sv1,sv2) of 
                (Value n1, Value n2) -> Value (n1 + n2)
                (Value n1, _) -> Add sv2 sv1
                otherwise -> if sv1 == sv2 then Mul sv1 (Value 2) else sv
        Minus sv1 sv2 -> 
            case (sv1,sv2) of 
                (Value n1, Value n2) -> Value (n1 - n2)
                (Value n1, _) -> Minus sv2 sv1
                (Minus sv1 (Value n1), Value n2) -> computeSV (Minus sv1 (Value (n1 +n2)))
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

printE :: Effect -> String
printE effect =
    case effect of 
        Bottom -> "_"
        Empty -> "e"
        Singleton str -> str
        Dot eff1 eff2 -> "(" ++ (printE eff1) ++ ". "++(printE eff2) ++")"
        OR eff1 eff2 -> "(" ++ (printE eff1) ++ " + "++(printE eff2) ++")"
        And eff1 eff2 -> "(" ++ (printE eff1) ++ " & "++(printE eff2) ++")"
        Star eff -> (printE eff) ++ "^*"
        Ttimes eff sv -> (printE eff) ++ "^" ++ (printSV sv)
        Omega eff -> (printE eff) ++ "^w"
        Neg eff  -> "!" ++ (printE eff) 
        

printS :: Singleton String -> String 
printS (Event str) =  show str
printS (NegEv str) =  show str

nullable :: Effect -> Bool
nullable effect = 
    case effect of 
        Bottom -> False
        Empty -> True
        Singleton _ -> False
        Dot e1 e2 -> nullable e1 && nullable e2
        OR e1 e2 -> nullable e1 || nullable e2
        And e1 e2 -> nullable e1 && nullable e2
        Star _ -> True 
        Neg e -> not (nullable e)
        Ttimes _ _ -> False
        Omega _ -> False



normal :: Effect -> Effect
normal effect =
     case effect of
        OR r s -> 
            if r == s then 
                --trace ("[Normal] " ++ printE effect ++ " ==> " ++ printE r)
                normal r
            else if r == Bottom then  
                --trace ("[Normal] " ++ printE effect ++ " ==> " ++ printE s)
                normal s
            else if s == Bottom then  
                --trace ("[Normal] " ++ printE effect ++ " ==> " ++ printE r)
                normal r 
            else OR (normal r) (normal s)
        
        Star eff ->  
            case eff of 
                Bottom -> 
                    --trace ("[Normal] " ++ printE effect ++ " ==> " ++ printE Bottom)
                    Bottom
                Empty -> 
                    --trace ("[Normal] " ++ printE effect ++ " ==> " ++ printE Empty)
                    Empty
                Star inner -> 
                    --trace ("[Normal] " ++ printE effect ++ " ==> " ++ printE (Star inner))
                    Star ( inner)
                otherwise -> effect
        Omega eff ->  
            case eff of 
                Bottom -> 
                    --trace ("[Normal] " ++ printE effect ++ " ==> " ++ printE Bottom)
                    Bottom
                Empty -> 
                    --trace ("[Normal] " ++ printE effect ++ " ==> " ++ printE Empty)
                    Empty
                Omega inner -> 
                    --trace ("[Normal] " ++ printE effect ++ " ==> " ++ printE (Star inner))
                    Star ( inner)
                otherwise -> effect
        And r s -> 
            if r == s then  
                --trace ("[Normal] " ++ printE effect ++ " ==> " ++ printE r)
                r 
            else if r == Bottom then 
                --trace ("[Normal] " ++ printE effect ++ " ==> " ++ printE Bottom)
                Bottom
            else if s == Bottom then 
                --trace ("[Normal] " ++ printE effect ++ " ==> " ++ printE Bottom)
                Bottom
            else effect
        Dot r s -> 
            if r == Empty then 
                --trace ("[Normal] " ++ printE effect ++ " ==> " ++ printE (normal s )) 
                normal s 
            else if s == Empty then 
                --trace ("[Normal] " ++ printE effect ++ " ==> " ++ printE (normal s )) 
                normal r 
            else if r == Bottom then 
                --trace ("[Normal] " ++ printE effect ++ " ==> " ++ printE Bottom)
                Bottom
            else Dot (normal r) (normal s)
        Neg e -> 
            case e of 
                Neg ee ->  e 
        Ttimes eff sv -> 
            case computeSV sv of  
                Value 0 -> Empty
                otherwise -> effect
                
        otherwise -> effect
        

intersection :: [Singleton String]  -> [Singleton String]  -> [Singleton String] 
intersection r s = 
    let helper eff effs acc = 
            case effs of
                [] -> acc 
                x:xs -> if x == eff then acc ++ [x] else helper eff xs acc
    in  foldr (\re acc->  helper re s acc) [] r 

union:: [Singleton String]  -> [Singleton String]  -> [Singleton String] 
union r s = 
    r ++ foldr (\se acc -> if elem se r then [] else [se]) [] s

inversion :: [Singleton String]  -> [Singleton String] 
inversion r = 
    map (\e -> 
        case e of 
            Event ee -> NegEv ee
            NegEv ee -> Event ee) r

-- get the first element
first :: Effect -> [Singleton String] 
first effect = 
    case effect of 
        Bottom -> []
        Empty -> []
        Singleton ev -> [Event ev]
        Dot e1 e2 -> 
            if nullable e1 then union (first e1) (first e2) 
            else first e1
        OR e1 e2 -> union (first e1) (first e2) 
        And e1 e2 -> intersection (first e1) (first e2 )
        Star r -> first r 
        Omega r -> first r 
        Neg e -> inversion (first e)
        Ttimes eff sv -> first eff


derivatives :: Effect -> Singleton String -> Effect
derivatives effect head = 
    case normal effect of 
        Bottom -> Bottom
        Empty -> Bottom
        Singleton a -> 
            if (head) ==  (Event a) then Empty 
            else if (head) ==  (NegEv a) then Empty
            else Bottom
        Dot e1 e2 -> 
            if nullable e1 then normal $ OR (normal $ Dot (derivatives e1 head)  e2) (derivatives e2 head)
            else 
                trace ("should be here!" ++ printE (normal $ Dot (derivatives e1 head) e2))
                normal $ Dot (derivatives e1 head) e2
        OR e1 e2 -> OR (derivatives e1 head) (derivatives e2 head)
        And e1 e2 -> And (derivatives e1 head) (derivatives e2 head)
        Star e -> normal (Dot (derivatives e head) effect )
        Omega e -> normal (Dot (derivatives e head) effect )
        Neg e -> Neg (derivatives e head)
        Ttimes eff sv -> Dot (normal (derivatives eff head)) (Ttimes eff (computeSV( Minus sv (Value 1))))

getAllIneq :: (Effect, Effect) -> Env
getAllIneq (r,s) =
    case (r,s) of 
        (Ttimes eff1 sv1, Ttimes eff2 sv2) -> 
            trace (printE (Ttimes eff1 (computeSV (Minus sv1 (Value 1)))) ++ "::: "++ printE ( Ttimes eff2 (computeSV (Minus sv2 (Value 1)))))
            [(r,s), (Ttimes eff1 (computeSV (Minus sv1 (Value 1))), Ttimes eff2 (computeSV (Minus sv2 (Value 1))))]
        otherwise -> [(r,s)]

unfold :: Effect -> Effect -> Env -> ([Tree String],Bool)
unfold r s env= 
    let headL = first r 
        nEvn = env ++ getAllIneq (r,s)
        helper h acc = 
             let (nodeNow, resultNow ) = acc
                 (node, result) = (containment  
                                    (normal $ derivatives (normal r) h) 
                                    (normal $ derivatives (normal s) h) nEvn)
             in 
                case node of
                    Node str list -> 
                        
                        ( nodeNow ++ [Node ("[Delete Head:" ++ printS h ++ "] " ++str) list] , resultNow && result)
             
    in  
        --trace ("Fist List: " ++ show headL)
             foldr helper ([] ,True) headL

printEntail :: Effect -> Effect -> String
printEntail eff1 eff2 = 
    (printE eff1) ++ " |- " ++ (printE eff2)


unfold_getResidue :: Effect -> Effect -> Env-> Maybe Effect
unfold_getResidue r s env=  --Nothing
    let headL = first r
        nEvn = env ++ [(r,s)]
        helper h = getResidue (normal $ derivatives (normal  r) h)  (normal $ derivatives (normal s) h)
    in foldr 
            (\h acc -> 
                case ((helper h nEvn), acc) of 
                    (Nothing, Nothing )-> Nothing
                    (Just e1 , Nothing )-> Just e1
                    (Nothing, Just e2 )-> Just e2
                    (Just e1, Just e2) -> Just (OR e1 e2)
            )
            (Nothing) headL
    

getResidue :: Effect -> Effect -> Env-> Maybe Effect
getResidue r s env=  
    case ((nullable r), (nullable s)) of
        -- disapprove 
        (True, False) -> 
            if r == Empty && s /= Bottom then Just s 
            else Nothing
        otherwise -> 
            let ifExist = (r,s) `elem` env
                normR = (normal r) 
                normS = (normal s)
            in 
                if ifExist then Nothing
                else 
                    --trace ("unfold_getResidue: " ++ printEntail normR  normS ) 
                    unfold_getResidue normR  normS env
            


containment :: Effect -> Effect -> Env -> (Tree String ,Bool)
containment r s env= 
    case ((nullable r), (nullable s)) of
        -- disapprove 
        (True, False) -> 
            --trace ("------------------------------------")
            trace ("GOAL: " ++ printEntail (normal r)  (normal s) ) 
            (Node ((printEntail (normal r)  (normal s) )++ " [Disprove!!!]") [] ,False)
        otherwise -> 
            let ifExist = (r,s) `elem` env
                normR = (normal r) 
                normS = (normal s)
                
            in  
                if ifExist then ((Node ((printEntail normR normS) ++ " [In context!!!]") []), True)
                else --trace ("------------------------------------")
                    let 
                        (nodes, result) = unfold normR normS env
                    in
                        trace ("other GOAL: " ++ printEntail normR normS) 
                        (Node (printEntail normR normS) nodes ,result)

append :: Effect -> Effect -> Effect
append eff1 eff2 =
    case eff1 of 
        Bottom ->  eff2
        Omega _ -> eff1
        _ -> Dot eff1 eff2
       
disjunction :: Effect -> Effect -> Effect
disjunction eff1 eff2 = OR eff1 eff2
    
destruct :: Effect -> (Effect, Effect)
destruct effect =
    case effect of 
        Bottom -> (Bottom, Bottom)
        Empty -> (Empty, Bottom)
        Dot e1 e2 -> 
            case e2 of 
                -- Omega oo -> 
                    -- trace ("I am here!")
                    --(e1, oo)
                otherwise -> (effect, Bottom)
        -- Omega oo -> (Bottom, oo)
        otherwise -> (effect, Bottom)


fixPoint ::  (Effect -> Effect)  -> Effect -> Effect 
fixPoint candy effect =
    if  effect ==  (candy effect) then effect
    else fixPoint  candy effect

righAsso :: Effect -> Effect 
righAsso effect = 
    case effect of 
        Dot (Dot e1 e2) e3 -> 
            --trace ("righAsso:" )
            fixPoint  righAsso (Dot (righAsso e1) (Dot (righAsso e2) (righAsso e3)))
        _ -> effect


pR :: Effect -> Effect -> (Tree String ,Bool) -> IO ()
pR r s (tree , res) = 
    if res then 
        do {
        putStrLn ("============= Report =============");
        putStrLn ("GOAL: " ++ printEntail r s);
        print "Succeed!" ;
        putStrLn $ drawTree tree
    }
    else 
        case getResidue r s [] of 
            Nothing -> 
                do {
                    putStrLn ("============= Report =============");
                    putStrLn ("GOAL: " ++ printEntail r s);
                    print "Failed.";
                    putStrLn $ drawTree tree
                }
            Just eff -> 
                do {
                    putStrLn ("============= Report =============");
                    putStrLn ("GOAL: " ++ printEntail r s);
                    print ("Succeed with a residue: "  ++ printE eff);
                    putStrLn $ drawTree tree
                }


