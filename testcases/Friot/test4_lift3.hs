
import Prelude 
import Basic
import Env
import Time
import Input
import Output
import Rpi

showTempTimeButton :: Int -> (Int, Int, Int) -> Bool-> String
showTempTimeButton n1 (m, d, h) b = show n1 ++ show m ++ show d ++ show h ++ show b

showLCD :: Signal String
showLCD = lift_3 showTempTimeButton (temprature 0) everySec (button 1)

main :: IO ()
main = bPlus [(lcd 1 showLCD)]

