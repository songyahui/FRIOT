import Antimirov

a :: Effect
a = Singleton "a"

b :: Effect
b = Singleton "b"

ab :: Effect 
ab = append a b  -- a;b

ba :: Effect 
ba = append b a  -- b;a

aba:: Effect 
aba = append a ba  -- a;b;a

bab :: Effect 
bab = append b ab  -- b;a;b

baa :: Effect 
baa = append ba a -- b;a;a

aab:: Effect 
aab = append a ab --a;a;b

ab_star:: Effect 
ab_star = Star ab --(ab) *

a_and_b :: Effect
a_and_b = disjunction a b   -- a \/ b

a_and_bStat :: Effect
a_and_bStat = Star a_and_b   -- (a \/ b) *

a_and_b_emp :: Effect
a_and_b_emp = disjunction Empty a_and_b -- e \/ a \/ b

a_star_b :: Effect
a_star_b = append (Star a) b -- a^*; b

b_a_star_b :: Effect
b_a_star_b = append b a_star_b -- b;a^*; b

b_star_a :: Effect
b_star_a = append (Star b) a -- b^*; a

a_b_star_a :: Effect
a_b_star_a = append a b_star_a --a;b^*; a

a_and_b_start_a :: Effect 
a_and_b_start_a = disjunction a b_star_a -- a \/ b^* ; a

b_a_star_b_and_a_b_star_a :: Effect 
b_a_star_b_and_a_b_star_a = disjunction b_a_star_b a_b_star_a -- b;a^*; b \/ a;b^*; a

verify_motion_sensor1 = 
    let eff_motion = (Dot (Star (Singleton "Passive")) (Singleton "Active")) 
        eff_if = (Singleton "Active")
        eff_else = append (Singleton "Passive") eff_motion
        final = disjunction eff_if eff_else
    in pR  final eff_motion (containment final eff_motion [])

verify_door_control = 
    let eff_motion = (Dot (Star (Singleton "Passive")) (Singleton "Active")) 
        a = append Bottom (Singleton "Close")
        b = append a eff_motion
        c = append b (Singleton "Open")
        d = append c (Singleton "Delay")
        eff_door = Star d
        final = append d eff_door
    in pR final eff_door (containment final eff_door [])

example1 =  -- n = 2
    let left = append (Star a_star_b ) (append (append a (append a a)) (Star a))
        right = append (Star a_and_b) (append a (append a_and_b a_and_b))
    in pR left right (containment left right [])

testL :: [Effect]
testL = [ --a,b,ab,ba,aba,
          bab, baa,aab,ab_star,a_and_b
        , append bab baa, append aab ab_star, append a_and_b ab_star , a_and_bStat
        , disjunction bab baa, disjunction aab ab_star, disjunction a_and_b ab_star 
        , a_and_b_emp,a_star_b,b_a_star_b,b_star_a,a_b_star_a
        , a_and_b_start_a, b_a_star_b_and_a_b_star_a ]

testL_product :: [(Effect,Effect)]
testL_product = [(x,y) | x <- testL, y <- testL]

test :: [(Effect,Effect)] -> IO()
test [] = return()
test (x:xs) = 
    let (a,b )=x
    in do{
        putStrLn ("***********************");
        putStrLn ("THE " ++ show (length xs) ++ "TH TEST CASE:");
        pR (a) (b) (containment a b []);
        test xs;
    }

main = test testL_product
