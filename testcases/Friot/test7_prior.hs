
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
showLCD = prior 1 (lift_2 (showTempTime) (temprature 0) (everySec))


main = bPlus [(lcd 1 showLCD)]

