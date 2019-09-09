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

data Effect 
    = Bottom 
    | Empty
    | Singleton String
    | Dot Effect Effect
    | OR Effect Effect 
    | And Effect Effect 
    | Star Effect 
    | Neg Effect  
    -- | Omega Effect 
    deriving (Show, Eq)

printE :: Effect -> String
printE effect =
    case effect of 
        Bottom -> "Ø"
        Empty -> "ϵ"
        Singleton str -> str
        Dot eff1 eff2 -> "(" ++ (printE eff1) ++ ". "++(printE eff2) ++")"
        OR eff1 eff2 -> "(" ++ (printE eff1) ++ " + "++(printE eff2) ++")"
        And eff1 eff2 -> "(" ++ (printE eff1) ++ " & "++(printE eff2) ++")"
        Star eff -> (printE eff) ++ "^*"
        -- Omega eff -> (printE eff) ++ "^w"
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


normal :: Effect -> Effect
normal effect =
     case effect of
        OR r s -> 
            if r == s then 
                trace ("[Normal] " ++ printE effect ++ " ==> " ++ printE r)
                r
            else if r == Bottom then  
                trace ("[Normal] " ++ printE effect ++ " ==> " ++ printE s)
                s
            else if s == Bottom then  
                trace ("[Normal] " ++ printE effect ++ " ==> " ++ printE r)
                r 
            else effect
        
        Star eff ->  
            case eff of 
                Bottom -> 
                    trace ("[Normal] " ++ printE effect ++ " ==> " ++ printE Bottom)
                    Bottom
                Empty -> 
                    trace ("[Normal] " ++ printE effect ++ " ==> " ++ printE Empty)
                    Empty
                Star inner -> 
                    trace ("[Normal] " ++ printE effect ++ " ==> " ++ printE (Star inner))
                    
                    Star ( inner)
                otherwise -> effect
        And r s -> 
            if r == s then  
                trace ("[Normal] " ++ printE effect ++ " ==> " ++ printE r)
                r 
            else if r == Bottom then 
                trace ("[Normal] " ++ printE effect ++ " ==> " ++ printE Bottom)
                Bottom
            else if s == Bottom then 
                trace ("[Normal] " ++ printE effect ++ " ==> " ++ printE Bottom)
                Bottom
            else effect
        Dot r s -> 
            if r == Empty then 
                trace ("[Normal] " ++ printE effect ++ " ==> " ++ printE (normal s )) 
                normal s 
            else if r == Bottom then 
                trace ("[Normal] " ++ printE effect ++ " ==> " ++ printE Bottom)
                Bottom
            else effect
        Neg e -> 
            case e of 
                Neg ee ->  e 
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
        Neg e -> inversion (first e)


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
            else normal $ Dot (derivatives e1 head) e2
        OR e1 e2 -> OR (derivatives e1 head) (derivatives e2 head)
        And e1 e2 -> And (derivatives e1 head) (derivatives e2 head)
        Star e -> normal (Dot (derivatives e head) effect )
        Neg e -> Neg (derivatives e head)

unfold :: Effect -> Effect -> ([Tree String],Bool)
unfold r s = 
    let headL = first r 
        helper h acc = 
             let (nodeNow, resultNow ) = acc
                 (node, result) = (containment  
                                    (normal $ derivatives (normal  r) h) 
                                    (normal $ derivatives (normal s) h))
             in 
                case node of
                    Node str list -> ( nodeNow ++ [Node ("[Head:" ++ printS h ++ "] " ++str) list] , resultNow && result)
             
    in  trace ("Fist List: " ++ show headL)
        foldr helper ([] ,True) headL

printEntail :: Effect -> Effect -> String
printEntail eff1 eff2 = 
    (printE eff1) ++ " |- " ++ (printE eff2)

containment :: Effect -> Effect -> (Tree String ,Bool)
containment r s = 
    case ((nullable r), (nullable s)) of
        -- disapprove 
        (True, False) -> 
            trace ("=================================")
            trace ("GOAL: " ++ printEntail r s) 
            (Node (printEntail r s) [] ,False)
        otherwise -> 
            if r == s then 
                trace ("=================================")
                trace ("r == s : " ++ printE r ) 
                (Node (printEntail r s) [] ,True)
            else 
                
                let (nodes, result) = unfold (normal r) (normal s)
                in  
                    trace ("=================================")
                    trace ("GOAL: " ++ printEntail r s) 
                    (Node (printEntail r s) nodes ,result)

append :: Effect -> Effect -> Effect
append eff1 eff2 =
    case eff1 of 
        Bottom ->  eff2
        -- Omega _ -> eff1
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


pR :: (Tree String ,Bool) -> IO ()
pR (tree , res) = 
    if res then 
        do {
        print "Success!" ;
        putStrLn $ drawTree tree
    }
    else print "Fail!"

verify_motion_sensor1 = 
    let eff_motion = (Dot (Star (Singleton "Passive")) (Singleton "Active")) 
        eff_if = (Singleton "Active")
        eff_else = append (Singleton "Passive") eff_motion
        final = disjunction eff_if eff_else
    in pR $ containment final eff_motion 

verify_door_control = 
    let eff_motion = (Dot (Star (Singleton "Passive")) (Singleton "Active")) 
        a = append Bottom (Singleton "Close")
        b = append a eff_motion
        c = append b (Singleton "Open")
        d = append c (Singleton "Delay")
        eff_door = Star d
        final = append d eff_door
    in pR $ containment final eff_door


