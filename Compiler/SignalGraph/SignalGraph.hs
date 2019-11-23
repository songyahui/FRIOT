module SignalGraph.SignalGraph where
import SignalGraph.DataType 
import Parser.AST 
import Data.Tree


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
                App "bPlus" [List el]  -> el
                otherwise -> []
        otherwise -> []
{--
getDef :: [Decl] -> String -> Decl --([Pattern], Expr)
getDef (x:xs) name =
    case x of 
        Definition n pl e -> 
            if name == n then Definition n pl e --(pl,e)
            else getDef xs name
        otherwise -> getDef xs name

find_type_anno:: [Decl] -> String -> Type
find_type_anno (x:xs) name = 
    case x of 
        Annotation n t -> -- Just TBool
            if n == name then t
            else find_type_anno xs name
        otherwise -> find_type_anno xs name
find_type_anno _ _ =  TString

   
   
getNodeFromDef :: [Decl] -> Decl -> SignalNode 
getNodeFromDef astp def = NoNode

let getNodeForOutput :: [Decl] -> String -> OutputNode
        getNodeForOutput astp name = 
            let def = getDef astp name 
            in getNodeFromDef astp def
    in 

--}

constructSignalTerm:: Expr -> SignalTerm
constructSignalTerm expr = 
    case expr of
        Signal (Int n) -> ConstInt n
        Signal (Str str) -> ConstStr str
        App "temprature" [Int n] -> Source "temprature" (TSignal TInt) n
        App "motion" [Int n] -> Source "motion" (TSignal TBool) n
        App "sound" [Int n] -> Source "sound" (TSignal TInt) n
        App "brightness" [Int n] -> Source "brightness" (TSignal TInt) n
        App "accelerator" [Int n] -> Source "accelerator" (Ttuple [TInt, TInt, TInt]) n
        App "button" [Int n] -> Source "button" (TSignal TBool) n
        -- Let [Def] Expr
        
        
        
        otherwise -> Nonode
        {--
        Let [Def] Expr
        Fold Expr Expr Expr
        Lift Expr Expr -- lift
        Lift2 Expr [Expr] -- lift2
        Lift3 Expr [Expr] -- lift3
        Sync Expr 
        Prior Int Expr 
        --}

constructGraph :: [Decl] -> Expr -> OutputNode
constructGraph astp expr = 
    case expr of 
        App "lcd" ((Int n1): e :es) ->
            ("lcd", TString , n1, constructSignalTerm e)
            -- IoN "lcd" TString e1 (getNodeForOutput astp (( method))) -- (tranExpr e2 "unknown" astp)
        App "led" ((Int n1):e:es) ->
            ("led", TBool , n1, constructSignalTerm e)
            -- IoN "led" TBool e1 (getNodeForOutput astp (( method)))   --(tranExpr e2 "unknown" astp)
        App "buzzer" ((Int n1):(e):es) ->
            ("buzzer", TBool , n1, constructSignalTerm e)
        App "fan" ((Int n1):(e):es) ->
            ("fan", TInt , n1, constructSignalTerm e)  
        App "alarm" ((Int n1):(e):es) ->
            ("alarm", Ttuple [TInt, TInt] , n1, constructSignalTerm e) 
        
signalGraph ::  [Decl] -> SignalGraph
signalGraph astp = 
    let outputs = getOutput astp
        graph = map (\e -> constructGraph astp e)  outputs
    in graph


