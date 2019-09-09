
import Debug.Trace
data Effect 
    = Nil 
    | Empty
    | Event String
    | Cons Effect Effect
    | OR Effect Effect -- (* eff1 \/ eff2 *)
    | Star Effect  -- (* event^* *)

    | Tevent Effect Int  -- (* event -> times -> effect *)
    | Omega Effect -- (* event^w *)
    deriving (Show, Eq)

equal :: Effect -> Effect -> Bool
equal eff1 eff2 =
    case  (eff1, eff2) of
      (Nil, Nil) -> True
      (Event a, Event b) -> a == b
      (Cons a1 b1, Cons a2 b2) -> (equal a1 a2) && (equal b1 b2)
      (Tevent e1 t1 , Tevent e2 t2)->
         if  equal e1 e2 then t1 == t2 else False
      (Star a, Star b) ->  equal a b
      (Omega a, Omega b) -> equal a b
      (OR a1 b1, OR a2 b2) -> 
          ((equal a1 a2) && (equal b1 b2))
          ||
          ((equal a1 b2) && (equal b1 a2))
      (_,_) -> False

printE :: Effect -> String
printE effect =
    case effect of 
        Nil -> ""
        Event str -> str
        Cons eff1 eff2 -> "(" ++ (printE eff1) ++ "; "++(printE eff2) ++")"
        Tevent eff num ->  (printE eff) ++ "^" ++ show num
        Star eff -> (printE eff) ++ "^*"
        Omega eff -> (printE eff) ++ "^w"
        OR eff1 eff2 -> "(" ++ (printE eff1) ++ " | "++(printE eff2)++")"

printEntail :: Effect -> Effect -> String
printEntail eff1 eff2 = 
    (printE eff1) ++ " |- " ++ (printE eff2)

append :: Effect -> Effect -> Effect
append eff1 eff2 =
    case eff1 of 
        Nil ->  eff2
        Omega _ -> eff1
        _ -> Cons eff1 eff2
       
disjunction :: Effect -> Effect -> Effect
disjunction eff1 eff2 =
    case (eff1, eff2) of
        --(Nil, _ ) -> eff2
        --(_, Nil ) -> eff1
        (e1, e2) -> OR e1 e2

normal :: Effect -> Effect
normal effect =
     case effect of
       -- (*% 1. emp; e === e  %*)
      Cons Nil eff -> 
        trace ("Normal 1: " ++ show (printE effect) ++ " === " ++ show (printE eff) ++ "          [emp; e === e]")
        normal eff 
       -- (*% 2. e; emp === e  %*)
      Cons eff Nil -> 
        trace ("Normal 2: " ++ show (printE effect) ++ " === " ++ show (printE eff) ++ "          [e; emp === e]")
        normal eff 
       {-- (*% 3. emp | e === e  %*)
      OR Nil eff -> 
        trace ("Normal 3: " ++ show (printE effect) ++ " === " ++ show (printE eff) ++ "          [emp \\/ e === e]")
        normal eff 
       -- (*% 4. e | emp === e  %*)
      OR eff Nil -> 
        trace ("Normal 4: " ++ show (printE effect) ++ " === " ++ show (printE eff) ++ "          [e \\/ emp === e]")
        normal eff 
       -- (*% 5. e1; (e2; e3) === (e1; e2); e3  %*)
       --}
      Cons eff1 (Cons eff2 eff3) -> 
        let res = fixPoint normal (Cons (Cons (normal eff1) (normal eff2)) (normal eff3))
        in 
            trace ("Normal 5: " ++ (show (printE effect)) ++ " === " ++ (show $ printE (res) ) ++ "          [e1; (e2; e3) === (e1; e2); e3]")
            res
       -- (*% 6. e1 \/ (e2 \/ e3) === (e1 \/ e2) \/ e3  %*)
      OR eff1 (OR eff2 eff3) -> 
        let res = OR (OR (normal eff1) (normal eff2)) (normal eff3)  
        in 
            trace ("Normal 6: " ++ (show (printE effect)) ++ " === " ++ (show $ printE (res) ) ++ "          [e1 \\/ (e2 \\/ e3) === (e1 \\/ e2) \\/ e3]")
            res
       -- (*% 7. e1; (e2 \/ e3) === (e1; e2) \/ (e1; e3)  %*)
      Cons eff1 (OR eff2 eff3) -> 
        let res = OR (Cons (normal eff1) (normal eff2)) (Cons (normal eff1) (normal eff3)) 
        in 
            trace ("Normal 7: " ++ (show (printE effect)) ++ " === " ++ (show $ printE (res) ) ++ "          [e1; (e2 \\/ e3) === (e1; e2) \\/ (e1; e3)]")
            res
        -- (*% 8. (e1 \/ e2); e3 === (e1; e3) \/ (e2; e3)  %*)     
      Cons (OR eff1 eff2) eff3-> 
        let res = OR (Cons (normal eff1) (normal eff2)) (Cons (normal eff2) (normal eff3)) 
        in 
            trace ("Normal 8: " ++ (show (printE effect)) ++ " === " ++ (show $ printE (res) ) ++ "          [(e1 \\/ e2); e3 === (e1; e3) \\/ (e2; e3)]")
            res
        -- (*| Cons eff1 eff2 => Cons (eff_normal eff1) (eff_normal eff2)   *)
      _ -> effect

fixPoint ::  (Effect -> Effect)  -> Effect -> Effect 
fixPoint candy effect =
    if equal effect (candy effect) then effect
    else fixPoint  candy effect

righAsso :: Effect -> Effect 
righAsso effect = 
    case effect of 
        Cons (Cons e1 e2) e3 -> 
            --trace ("righAsso:" )
            fixPoint  righAsso (Cons (righAsso e1) (Cons (righAsso e2) (righAsso e3)))
        _ -> effect

consume_Omega :: Effect -> Effect -> Effect
consume_Omega effR effL  =
    if testEff effL effR then Omega effR
    else 
      let res = righAsso effL    
      in 
        if testEff res effR  then 
            --trace ("consume_Omega:" ++ show (printE (Omega effR)) )
            Omega effR
        else 
            case res of 
                Cons e1 e2 -> 
                    let res = subsume (Cons e1 (consume_Omega effR e2 ))
                    in  trace ("Residue:" ++ show (printE (res)))
                        res
                _ -> Cons effL (Omega effR)

consume_Star :: Effect -> Effect -> Effect
consume_Star effR effL  =
    if testEff effL effR then Omega effR
    else 
      let res = righAsso effL    
      in 
        if testEff res effR  then 
            --trace ("consume_Star:" ++ show (printE (Star effR)) )
            Star effR
        else 
            case res of 
                Cons e1 e2 -> 
                    let res = subsume (Cons e1 (consume_Star effR e2 ))
                    in  --trace ("consume_StarE:" ++ show (printE (res)))
                        res
                _ -> Cons effL (Star effR)
        
        --trace (show (printE res) ++ "     " ++show (printE effR) )
        --Nil


subsume ::  Effect -> Effect
subsume effect =
  case effect of
    
    Cons effL (Omega effoR) -> 
        let res =consume_Omega effoR effL 
        in trace ("------------------------") 
            trace ("Subsume w: " ++ (show (printE effect) ++ " ---> " ++ (show $ printE (res) )  ))   
            res
    Cons effL (Star effoR) -> 
        let res =consume_Star effoR effL 
        in trace ("------------------------") 
            trace ("Subsume *: " ++ (show (printE effect) ++ " ---> " ++ (show $ printE (res) )  ))   
            res
       
    Cons (Tevent eff1 t1) (Tevent eff2 t2 ) ->
       if equal (normal eff1) (normal eff2) then 
            trace ("------------------------") 
            trace ("Subsume t1+t2: " ++ (show (printE effect)) ++ " ---> "++ show (printE (Tevent (normal eff1) ( t1  + t2))) )          
            Tevent (normal eff1) ( t1  + t2)
       else effect
     --(*% 4. e; e^t == e^t+1  %*) 
    Cons effa (Tevent effb t) -> if equal effa effb then 
            trace ("------------------------") 
            trace ("Subsume t+1: " ++ (show (printE effect)) ++ " ---> "++ show (printE (Tevent effb (t+1)) ) )                
            (Tevent effb (t+1)) 
        else Cons (subsume effa) (Tevent (subsume effb) t)
     --(*% 5. e^w ; _ == e^w  %*)      
    Cons (Omega effo) _ -> 
        trace ("------------------------") 
        trace ("Subsume w abanden: " ++ (show (printE effect)) ++ " ---> "++ show (printE (Omega effo) ) ++ "          [e^w ; _ == e^w ]")                
        (Omega effo)

    
    Cons eff1 eff2 -> Cons (subsume eff1) (subsume eff2)
    _ -> effect

entail :: Effect ->  Effect -> Bool 
entail eff1 eff2 =
  if equal eff1 eff2 then True 
  else 
   case (eff1, eff2) of
     -- (*% 2. e1 \/ e2 |- e1/e2  %*)
    (OR eff1a eff1b, eff22) -> 
        trace ("-----------------------------------")
        trace ("Entail 1: " ++(printEntail eff1a eff22) ++ "  or  " ++ printEntail eff1b eff22 ++ "         [e1 \\/ e2 |- e3  <=> e1 |- e3 \\/ e2 |- e3]") 
        testEff eff1a eff22 || testEff eff1b eff22
     --(*% 1. e1 |- e2^*; e1  %*) 
    (effa, Cons (Star _) (effb)) -> 
        if equal effa effb then 
            trace ("-----------------------------------")
            trace ("Entail 2: " ++(printEntail eff1 eff2)  ++ "         [e1 |- e2^*; e1]") 
            True 
        else False
     -- (*% 3. e1/e2 |- e1 \/ e2  %*)
    ( eff11, OR eff2a eff2b) -> 
        --trace ("-----------------------------------")
        trace ("Entail 3: " ++(printEntail eff11 eff2a) ++ "  or  " ++ printEntail eff11 eff2b ++ "         [e1/e2 |- e1 \\/ e2]") 
        entail eff11 eff2a || entail eff11 eff2b
    ( _,_) -> False

testEff :: Effect ->  Effect -> Bool 
testEff eff1 eff2 =
   --(* step 1. eff_normal
   --   step 2. eff_sub
   --   step 3. eff_entail
   -- *)
   trace ("=================================")
   trace ("GOAL: " ++ printEntail eff1 eff2) 
   entail (subsume (normal eff1)) (subsume (normal eff2))
   -- trace ("=================================")

pR :: Bool -> IO ()
pR res = 
    if res then print "Success!" else print "Fail!"

verify_motion_sensor1 = 
    let eff_motion = (Cons (Star (Event "Passive")) (Event "Active")) 
        eff_if = (Event "Active")
        eff_else = append (Event "Passive") eff_motion
        final = disjunction eff_if eff_else
    in pR $ testEff final eff_motion

verify_motion_sensor2 = 
    let eff_motion = (Cons (Star (Event "Passive")) (Event "Active")) 
        eff_if = (Event "Active")
        eff_else = append (Event "Passive") eff_motion
        final =disjunction eff_else eff_if
    in pR $ testEff final eff_motion


verify_door_control = 
    let eff_motion = (Cons (Star (Event "Passive")) (Event "Active")) 
        a = append Nil (Event "Close")
        b = append a eff_motion
        c = append b (Event "Open")
        d = append c (Event "Delay")
        eff_door = Omega d
        final = append d eff_door
    in pR $ testEff final eff_door

verify_delay t = 
    let eff_delay = (Tevent (Event "Tick") t) 
        eff_if = Nil
        eff_elseA = append Nil (Event "Tick")
        eff_elseB = append eff_elseA (Tevent (Event "Tick") (t-1))
        final = disjunction eff_if eff_elseB
    in pR $ testEff (final) (eff_delay)

verify_light_control t= 
    let 
        eff_until_ready = (Cons (Star (Event "Wait")) (Event "Ready")) 
        eff_delay = (Tevent (Event "Tick") t) 
        eff_light_controlA = append (Event "Ready") (Tevent (Event "Tick") t) 
        eff_light_controlB = (Star (Event "Wait"))
        eff_light_control = Omega (disjunction eff_light_controlA eff_light_controlB)
        a = append Nil eff_until_ready
        b = append a eff_delay  
        final = append b eff_light_control

    in pR $ testEff final eff_light_control

test t= 
    let 
        eff_until_ready = (Cons (Star (Event "Wait")) (Event "Ready")) 
        eff_delay = (Tevent (Event "Tick") t) 
        eff_light_controlA = append (Event "Ready") (Tevent (Event "Tick") t) 
        eff_light_controlB = (Star (Event "Wait"))
        eff_light_control = Omega (disjunction eff_light_controlA eff_light_controlB)
        a = append Nil eff_until_ready
        b = append a eff_delay  
        final = append b eff_light_control

    in pR $ testEff final eff_light_controlB


or_normal = 
    let 
        left = Star (OR (Event "A") (Event "B")) 
        right = Cons  (Star (Event "A")) (Star(Cons (Event "B") (Star (Event "A")) ))
    in pR $ testEff  left right



{--

-------------------------
OR (Event "Active") (Cons (Event "Passive") (Cons (Star (Event "Passive")) (Event "Active"))) |- Cons (Star (Event "Passive")) (Event "Active")
-------------------------
(Active | (Passive; (Passive^*; Active))) |- (Passive^*; Active)


--}

