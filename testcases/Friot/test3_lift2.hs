
import Prelude 
import Basic
import Env
import Time
import Input
import Output
import Rpi

showTempTime :: Int -> (Int, Int, Int) -> String
showTempTime n1 (m, d, h) = show n1 ++ show m ++ show d ++ show h

showLCD :: Signal String
showLCD = lift_2 showTempTime (temprature 0) everySec

main :: IO ()
main = bPlus [(lcd 1 showLCD)]

{--

showTemp :: Int -> Int -> String
showTemp t temp = 
    if t == 0 then (show temp)
    else (showTemp (t -1) temp)

showLCD :: Signal String
showLCD = lift2 showTemp temprature everySec

main :: IO ()
main = bPlus [(Basic.lcd 0 showLCD)]



showTemp :: Int -> Int -> String
showTemp t temp = 
    if t == 0 then ev "Ready" (show temp)
    else ev "Wait" (showTemp (t -1) temp)
    



test step count =  effect "song" (count + 1)



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



mode_LCD :: Signal Bool
mode_LCD = lift (\a -> if a then True else False) isPeopleIn


meth :: Bool ->Bool
meth = \a -> if a then True else False

main = bPlus [(lcd 2 (lcd_show)),(led 3 isPeopleIn),(lcd 4 (lcd_show1))]


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