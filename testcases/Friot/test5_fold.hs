
import Prelude 
import Basic
import Env
import Time
import Input
import Output
import Rpi



peoplecount :: Signal Int
peoplecount = fold  (\a acc -> acc +1) 0 (motion 0)  

lcd_show :: Signal String
lcd_show = lift (\a -> toStr a) peoplecount


main = bPlus [(lcd 1 lcd_show)]

