module Time where
import Basic

data Time = HMS (Int,Int,Int)
    
everySec :: Signal Time
everySec = Signal $ HMS (0,0,0)