
import Prelude hiding (show)
import Basic
import Env
import Time
import Rpi



{-@ 
test step count <>
requires emp 
ensures  (("song")^100)
@-}
test step count =  effect "song" (count + 1)

{-@ 
omega step count <>
requires emp 
ensures  (("song")w)
@-}
omega step count = effect "song" (count + 1)


{-@ 
getReady a <> 
requires emp 
ensures  ((("Wait")^a).("Ready")) +  (("Wait")*)
@-}
getReady a = if a < 0 then (effect "Ready" (a)) else (effect "Wait" getReady (a-1))



peoplecount :: Signal Int
peoplecount = fold  (test) 0 (motion 0)  

isPeopleIn :: Signal Bool
isPeopleIn = lift (meth) (motion 0)

lcd_show :: Signal String
lcd_show = lift_2 (\a b -> if a then (toStr b) else "null") (mode_LCD) (temprature 1)        


lcd_show2 :: Signal String
lcd_show2 = sync (lift_2 (\a b -> if a then (toStr b) else "null") (mode_LCD) (temprature 1)  )        


lcd_show3 :: Signal String
lcd_show3 = prior 3 (lcd_show)


lcd_show4 :: Signal String
lcd_show4 = prior 3 (sync (lcd_show)     )   


lcd_show1 :: Signal String
lcd_show1 = lift (\a -> toStr a) peoplecount


mode_LCD :: Signal Bool
mode_LCD = lift (\a -> if a then True else False) isPeopleIn


meth :: Bool ->Bool
meth = \a -> if a then True else False

main = bPlus [(lcd 2 (lcd_show)),(led 3 isPeopleIn),(lcd 4 (lcd_show1))]


{--



lcd_show :: Signal String
lcd_show = lift_2 (\a b -> if a then toStr b else "null") (mode_LCD) (Env.temprature 1)        


lcd_show1 :: Signal String
--lcd_show1 = lift (\a -> toStr a) peoplecount




meth :: Bool ->Bool

meth1 :: Bool -> Signal Bool -> Signal Bool -> Signal Int
--meth = \a -> if a then True else False

--main = Rpi.bPlus [(lcd 2 (lcd_show)),(led 3 isPeopleIn),(lcd 4 (lcd_show1))]


main :: IO ()
main = Rpi.bPlus [(lcd 2 (lcd_show)),(led 3 isPeopleIn),(lcd 4 (lcd_show1))]


                 


--}