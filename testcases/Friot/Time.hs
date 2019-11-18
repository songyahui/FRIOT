module Time where
import Basic

type Time = (Int,Int,Int)
    
everySec :: Signal Time
everySec = Signal $ (0,0,0)