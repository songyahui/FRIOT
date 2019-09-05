module Basic where

data Signal a = Signal a

lift :: (a -> b ) -> Signal a  -> Signal b
lift fun (Signal a) =  Signal (fun a )
    
lift_2 :: (a -> b -> c) -> Signal a -> Signal b -> Signal c
lift_2 fun (Signal a) (Signal b) =  Signal (fun a b)

lift_3 :: (a -> b -> c ->d) -> Signal a -> Signal b -> Signal c -> Signal d
lift_3 fun (Signal a) (Signal b) (Signal c) =  Signal (fun a b c)

foldP :: (a -> b -> b) -> b -> Signal a -> Signal b
foldP fun b (Signal a) = Signal (fun a b)

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




  
