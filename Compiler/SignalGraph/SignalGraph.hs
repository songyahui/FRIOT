module SignalGraph.SignalGraph where
import SignalGraph.DataType 
import Parser.AST 


getOutput :: [Decl] -> [Expr]
getOutput astp = 
    let main_fun decl = 
            case  decl of 
                Definition "main" _ _ -> True
                otherwise -> False
        main_ = filter main_fun astp 
    in case main_ !! 0 of 
        Definition "main" p l -> 
            case l of 
                Call "Rpi.bPlus" [List el]  -> el
                otherwise -> []
        otherwise -> []

getDef :: [Decl] -> String -> ([Pattern], Expr)
getDef (x:xs) name =
    case x of 
        Definition n pl e -> 
            if name == n then (pl,e)
            else getDef xs name
        otherwise -> getDef xs name

find_type_anno:: [Decl] -> String -> Type
find_type_anno (x:xs) name = 
    case x of 
        Annotation n t -> -- Just TBool
            if n == name then t
            else find_type_anno xs name
        otherwise -> find_type_anno xs name
find_type_anno _ _ =  TVar "SONG"

mothToExpr:: [Decl] -> Expr -> Expr
mothToExpr astp expr =
    case expr of 
        Parent e -> mothToExpr astp e
        Var name -> 
            let (pl, _expr) = getDef astp name 
            in mothToExpr astp _expr
        otherwise -> expr
        
        
tranExpr :: Expr -> String -> [Decl] -> SignalNode
tranExpr expr name astp=
    let _type = find_type_anno astp name
    in case expr of 
        Parent e -> tranExpr e name astp
        Var name -> 
            let (pl, _expr) = getDef astp name 
            in tranExpr _expr name astp
        Call "Env.motion" pl -> Source "Env.motion" TBool pl 
        Call "Env.temprature" pl -> Source "Env.temprature" TInt pl 
        Call "lift" (meth:pl) -> 
            let chileN = map (\e -> tranExpr e "unknown" astp) pl
            in LiftN name 1 (mothToExpr astp meth) _type chileN
        Call "lift_2" (meth:pl) -> 
            let chileN = map (\e -> tranExpr e "unknown" astp) pl
            in LiftN name 2 (mothToExpr astp meth) _type chileN
        Call "foldP" (meth:acc:pl) -> 
            let chileN = map (\e -> tranExpr e "unknown" astp) pl
            in FoldP name (mothToExpr astp meth) _type acc (head chileN)

            
constructGraph :: [Decl] -> Expr -> SignalNode
constructGraph astp expr = 
    case expr of 
        Parent e -> constructGraph astp e 
        Call "lcd" (e1:e2:es) ->
            IoN "lcd" TString e1 (tranExpr e2 "unknown" astp)
        Call "led" (e1:e2:es) ->
            IoN "led" TBool  e1 (tranExpr e2 "unknown" astp)

        
signalGraph ::  [Decl] -> SignalGraph
signalGraph astp = 
    let outputs = getOutput astp
        graph = map (\e -> constructGraph astp e)  outputs
    in SG graph