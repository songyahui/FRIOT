
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
lcd_show = Signal "test"

isPeopleIn :: Signal Bool
isPeopleIn = lift (meth) (motion 0)


main :: IO ()
main = Rpi.bPlus [(lcd 2 (lcd_show)),(led 3 isPeopleIn)]


