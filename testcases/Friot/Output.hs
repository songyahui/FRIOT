module Output where
import Basic

lcd :: Int -> Signal String -> IO()
lcd port (Signal str) = print 1

led :: Int -> Signal Bool -> IO()
led port (Signal str) = print 1

buzzer :: Int -> Signal Bool -> IO()
buzzer port (Signal str) = print 1

fan :: Int -> Signal Int -> IO()
fan port (Signal level) = print 1

alarm :: Int -> Signal (Int, Int) -> IO()
alarm port (Signal (n1,n2)) = print 1