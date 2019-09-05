module Rpi where
import Basic

bPlus:: [IO()] -> IO() 
bPlus a = print 1

lcd :: Int -> Signal String -> IO()
lcd port (Signal str) = print 1

led :: Int -> Signal Bool -> IO()
led port (Signal str) = print 1