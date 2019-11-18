
import Prelude 
import Basic
import Env
import Time
import Input
import Output
import Rpi

showLCD :: Signal String
showLCD = (lift (show) (temprature 0))

main = bPlus [(lcd 1 showLCD)]
