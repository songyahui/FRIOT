
import Prelude 
import Basic
import Env
import Time
import Input
import Output
import Rpi


main :: IO ()
main = bPlus [(lcd 1 (Signal "test"))]
