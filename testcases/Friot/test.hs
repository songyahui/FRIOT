import Prelude hiding (show)
import Basic
import Env
import Time
import Rpi

peoplecount :: Signal Int
peoplecount = foldP  (\step count -> count + 1) 0 (Env.motion 0)    

main :: IO ()
main = Rpi.bPlus [(lcd 2 (lcd_show)),(led 3 isPeopleIn),(lcd 4 (lcd_show1))]

meth :: Bool -> Bool
meth = \a -> if a then True else False
                 
isPeopleIn :: Signal Bool
isPeopleIn = lift (meth) (Env.motion 0)

lcd_show :: Signal String
lcd_show = lift_2 (\a b -> if a then toStr b else "null") (mode_LCD) (Env.temprature 1)        


lcd_show1 :: Signal String
lcd_show1 = lift (\a -> toStr a) peoplecount


mode_LCD :: Signal Bool
mode_LCD = lift (\a -> if a then True else False) isPeopleIn

{-- 


home :: IO ()
home = Rpi.bPlus [(lcd 1 show)]

peoplecount :: Signal Int
peoplecount = foldP (compute) 0 (Env.motion 0) 

show1 :: Signal String
show1 = lift_2 strcat peoplecount Time.everySec 

blink :: Signal Bool
blink = foldP (\a state -> not state) False Time.everySec 

peoplecount :: Signal Int

show :: Signal String
show = lift_2 strcat (Env.temprature 0) Time.everySec 

compute :: a -> Int -> Int
compute a b = 0

isPeopleIn :: Signal Bool
isPeopleIn = lift (\a -> if a then True else False) (Env.motion 0)

mode_LCD :: Signal Bool
mode_LCD = lift (\a -> if a then True else False) isPeopleIn

lcd_show :: Signal String
lcd_show = lift_2 (\a b -> if a then toStr b else "null") mode_LCD (Env.temprature 1) 

--}