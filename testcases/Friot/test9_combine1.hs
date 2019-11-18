
import Prelude 
import Basic
import Env
import Time
import Input
import Output
import Rpi


meth :: Bool ->Bool
meth = \a -> if a then True else False


lcd_show :: Signal String
lcd_show = prior 5 (Signal "test")

isPeopleIn :: Signal Bool
isPeopleIn = prior 3 (lift (meth) (motion 0))


main = bPlus [(lcd 2 (lcd_show)),(led 3 isPeopleIn)]


