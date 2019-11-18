module Basic where

data Signal a = Signal a

ev:: String -> b -> b
ev name re = re

effect :: String -> a -> a
effect str a = a 

lift :: (a -> b ) -> Signal a  -> Signal b
lift fun (Signal a) =  Signal (fun a )

sync :: Signal a -> Signal a
sync a = a

prior :: Int  -> Signal a -> Signal a
prior level a = a 
    
lift_2 :: (a -> b -> c) -> Signal a -> Signal b -> Signal c
lift_2 fun (Signal a) (Signal b) =  Signal (fun a b)

lift_3 :: (a -> b -> c ->d) -> Signal a -> Signal b -> Signal c -> Signal d
lift_3 fun (Signal a) (Signal b) (Signal c) =  Signal (fun a b c)

lift_4 :: (a -> b -> c ->d -> e) -> 
    Signal a -> Signal b -> Signal c -> Signal d -> Signal e
lift_4 fun (Signal a) 
            (Signal b) 
            (Signal c) 
            (Signal d) =  
                Signal (fun a b c d)

lift_5 :: (a -> b -> c ->d -> e->f) -> 
    Signal a -> Signal b -> Signal c -> Signal d -> Signal e -> Signal f
lift_5 fun (Signal a) 
    (Signal b) 
    (Signal c) 
    (Signal d) 
    (Signal e) =  
        Signal (fun a b c d e )

lift_6 :: (a -> b -> c ->d -> e->f ->g) -> 
    Signal a -> Signal b -> Signal c -> 
        Signal d -> Signal e -> Signal f -> Signal g
lift_6 fun (Signal a) 
    (Signal b) 
    (Signal c) 
    (Signal d) 
    (Signal e) 
    (Signal f) =  
        Signal (fun a b c d e f)


lift_7 :: (a -> b -> c ->d -> e->f ->g->h) -> 
    Signal a -> Signal b -> Signal c -> 
        Signal d -> Signal e -> Signal f -> Signal g -> Signal h
lift_7 fun (Signal a) 
    (Signal b) 
    (Signal c) 
    (Signal d) 
    (Signal e) 
    (Signal f) 
    (Signal g) =  
        Signal (fun a b c d e f g)


lift_8 :: (a -> b -> c ->d -> e->f ->g->h -> i) -> 
    Signal a -> Signal b -> Signal c -> 
        Signal d -> Signal e -> Signal f -> Signal g -> Signal h -> Signal i
lift_8 fun (Signal a) 
    (Signal b) 
    (Signal c) 
    (Signal d) 
    (Signal e) 
    (Signal f) 
    (Signal g) 
    (Signal h) =  
        Signal (fun a b c d e f g h)

fold :: (a -> b -> b) -> b -> Signal a -> Signal b
fold fun b (Signal a) = Signal (fun a b)

strcat:: a -> b -> String
strcat a b = "showlcd"


toStr :: Int -> String
toStr a = "str"

instance Functor Signal where
    fmap fun (Signal a) = Signal $ fun a

instance Applicative Signal where
    pure a = Signal a
    (<*>) (Signal fun) (Signal a) = (Signal $ fun a)

instance Monad Signal where
    return a = Signal a  
    (>>=) (Signal a) fun = (fun a) 

