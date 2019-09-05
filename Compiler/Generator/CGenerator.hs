module Generator.CGenerator where
import Parser.AST
import SignalGraph.DataType
import Data.Char (toLower, toUpper)


countNum :: SignalNode -> Int
countNum node =
    case node of 
        Source _ _ _ -> 1  -- input signal type, param, name 
        LiftN _ _ _ _  nodeL ->1 + (foldr (+) 0 (map countNum nodeL)) -- liftn 
        FoldP _ _ _ _ node -> 1+ (countNum node)
        IoN _ _ _ node -> 1+ (countNum node)

threadFrame :: Int-> String -> String
threadFrame index content = 
    "\nvoid* thread"++ (show index ) ++ "(void *arg){\n" ++content ++"return NULL;\n}\n"

whileFrame :: String -> String
whileFrame content = 
    "while (1){\n" ++content ++"delay (100) ;\n}\n"

ifFrame :: String -> String -> String
ifFrame cond contend = 
    "if (" ++ cond++ ") { \n " ++ contend ++ "\n}\n"

transNM :: [Char] -> String
transNM [] = []
transNM (x:xs) = 
    case x of 
        '.' -> "_" ++ transNM xs 
        otherwise -> [x] ++ transNM xs 

getPort  :: [Expr] -> String       
getPort epl =
    case head epl of 
        Int num -> show num

tyTostr :: Type -> String
tyTostr tt = 
    case tt of 
        TBool -> "int "
        TInt -> "int "
        TString -> "char * "
        TApp [TVar "Signal",TString] -> "char * "
        TApp [TVar "Signal",TBool] -> "int "
        TApp [TVar "Signal",TInt] -> "int "

exprT_Friot_C ::Expr -> String
exprT_Friot_C expr = 
    case expr of 
        Var name -> name 
        Boolean "True"  -> "return " ++ "1;"
        Boolean "False" -> "return " ++"0;"
        Int num -> show num ++";"
        Str name -> "return " ++ "\"" ++ name ++ "\";" 
        Call caller exprL -> "return " ++ caller ++ "("++ flatten (map exprT_Friot_C exprL) ++");\n"
        Parent e -> exprT_Friot_C e
        If ifL else_e -> 
            let ifs = flatten $ map (\(con, th) -> "if (" ++ (exprT_Friot_C con) ++ ")\n{\n " ++ (exprT_Friot_C th ) ++ "\n}\n") ifL
                else_ = "else{\n " ++  (exprT_Friot_C else_e) ++ "\n}\n"
            in ifs ++ else_
        --otherwise -> "YAHUI"

get_ty_from_Node :: SignalNode -> String
get_ty_from_Node node = 
    case node of 
        IoN nm ty _ _ -> (tyTostr ty) 
        FoldP nm _ ty _ _ -> (tyTostr ty) 
        LiftN nm _ _ ty _ -> (tyTostr ty) 
        Source nm ty _ ->(tyTostr ty) 

patternTran :: Pattern -> String
patternTran p =
    case p of 
        PStr name -> name
        PInt num -> show num
        PVar num -> num
        
getPattern_args::[Pattern] -> [SignalNode] ->String
getPattern_args [p] [n] = get_ty_from_Node n ++ patternTran p
getPattern_args (p:ps) (n:ns) = get_ty_from_Node n ++ patternTran p ++ "," ++ (getPattern_args ps ns) 
     

creatMeth :: String -> String-> Expr -> [SignalNode] -> String
creatMeth ty nm expr nodeL=
    case expr of 
        Parent e -> creatMeth ty nm e nodeL
        Lambda pl exp -> 
            let args = getPattern_args pl nodeL
                body = exprT_Friot_C exp
            in ty ++ nm++"_meth" ++ "("++ args ++ ")\n{"++body ++"\n}\n" 


getNode_Name_args :: [SignalNode] -> String
getNode_Name_args [x] = 
    case x of 
        IoN nm _ _ _ -> "model." ++ map toLower (transNM nm)
        FoldP nm _ _ _ _ -> "model." ++ map toLower nm
        Source nm _ _ -> "model." ++ map toLower (transNM nm)
        LiftN nm _ _ _ _ -> "model." ++ map toLower nm
getNode_Name_args (x:xs) = 
    case x of 
        IoN nm _ _ _ -> "model." ++ map toLower (transNM nm) ++ "," ++ getNode_Name_args xs
        FoldP nm _ _ _ _ -> "model." ++ map toLower nm ++ ","++ getNode_Name_args xs
        Source nm _ _ -> "model." ++ map toLower (transNM nm)++ ","++ getNode_Name_args xs
        LiftN nm _ _ _ _ -> "model." ++ map toLower nm++ ","++ getNode_Name_args xs

digitalRwrite :: String -> String -> String -> String
digitalRwrite dev device_n model_n = 
    case dev of 
        ("lcd") -> "lcdPuts(" ++ device_n ++ "," ++ model_n ++ ");\n"
        otherwise ->"digitalWrite(" ++ device_n ++ "," ++ model_n ++ ");\n"

threadGen :: Int -> SignalNode -> String
threadGen index node = 
    case node of 
        Source nm dt epl -> 
            let device_n = map toUpper (transNM nm)
                model_n = "model." ++ map toLower device_n
                pinMode = "pinMode ("++ device_n ++", INPUT) ;\n"
                didgitR = "int value = digitalRead(" ++ device_n ++ ");\n"
                if_else = ifFrame ("value!=" ++model_n ) (model_n ++ "=value;\n")
                def_device = "#define " ++ device_n ++ " " ++ getPort epl ++ "\n"
            in 
                def_device ++ threadFrame index (pinMode ++ (whileFrame (didgitR ++if_else)))
        LiftN nm num meth ty nodeL ->
            let _ty = tyTostr ty 
                def_meth = creatMeth _ty nm meth nodeL
                args = getNode_Name_args nodeL 
                callMeth = nm++"_meth (" ++ args ++ ");\n"
                didgitR = _ty ++ " value  = "++callMeth++"\n"
                model_n = "model." ++ map toLower nm
                update = model_n ++ " = value;" 
            in def_meth ++ threadFrame index (whileFrame (didgitR ++ update))
        FoldP nm _ _ _ _ -> 
            threadFrame index (whileFrame (""))
        IoN nm ty port node -> 
            let device_n = (map toUpper (transNM nm)) ++ "_"++ (getPort [port])
                model_n = "model." ++ map toLower nm ++ "_"++ (getPort [port])
                _ty = tyTostr ty
                didgitR = digitalRwrite nm device_n model_n
                pinMode = "pinMode ("++ device_n ++", OUTPUT) ;\n"
                def_device = "#define " ++ device_n ++ " " ++ getPort [port] ++ "\n"
            
            in def_device ++ threadFrame index (pinMode ++ (whileFrame (didgitR)))
        



flatten:: [[a]] -> [a]
flatten list =
    let helper acc [] = acc
        helper acc (x:xs) = acc ++ x ++ helper [] xs 
    in helper [] list

flattenST :: SignalNode -> [SignalNode]
flattenST sn =
    case sn of 
        IoN _ _ _ node -> [sn] ++ (flattenST node)
        FoldP _ _ _ _ node -> [sn] ++ (flattenST node)
        Source _ _ _ -> [sn]
        LiftN _ _ _ _ nodeL -> [sn] ++ flatten (map flattenST nodeL )
   
isIn :: SignalNode  -> [SignalNode] -> Bool
isIn _ [] = False
isIn n (x:xs) = 
    if n == x then True
    else isIn n xs

deleteR :: [SignalNode] -> [SignalNode]
deleteR [] = []
deleteR (x:xs) =
    if isIn x xs then deleteR xs
    else [x] ++ deleteR xs

zipNode_Index :: [SignalNode] -> [(Int, SignalNode)]
zipNode_Index list =
    let helper num [] =  []
        helper num (x:xs) = [(num, x)] ++ helper (num + 1) xs 
    in helper 0 list


model_Gen :: [SignalNode] -> String
model_Gen list =
    let tyToinital tt = 
            case tt of 
                TBool -> "0"
                TInt -> "0"
                TString -> "\"nothing\""
                TApp [TVar "Signal",TString] -> "\"nothing\""
                TApp [TVar "Signal",TBool] -> "0"
                TApp [TVar "Signal",TInt] -> "0"

        struct [] acc = acc
        struct (x:xs) acc = 
            let _ty = 
                 case x of 
                    IoN nm ty expr _ -> (tyTostr ty) ++ (map toLower nm) ++ "_"++ (getPort [expr])
                    FoldP nm _ ty _ _ -> (tyTostr ty) ++ (map toLower nm)
                    LiftN nm _ _ ty _ -> (tyTostr ty) ++ (map toLower nm)
                    Source nm ty _ -> (tyTostr ty) ++ (map toLower (transNM nm))
            in struct xs (acc ++ _ty ++ ";\n")
        initial [x] acc = 
            acc ++ (
                case x of 
                    IoN nm ty _ _ -> (tyToinital ty) 
                    FoldP nm _ ty _ _ -> (tyToinital ty) 
                    LiftN nm _ _ ty _ -> (tyToinital ty) 
                    Source nm ty _ ->(tyToinital ty) 
            ) 
        initial (x:xs) acc = 
            let _ty = 
                 case x of 
                   IoN nm ty _ _ -> (tyToinital ty) ++ ","
                   FoldP nm _ ty _ _ -> (tyToinital ty) ++ ","
                   LiftN nm _ _ ty _ -> (tyToinital ty) ++ ","
                   Source nm ty _ ->(tyToinital ty) ++ ","
            in initial xs (acc ++ _ty )
            
        d_struct = "typedef struct _Model{\n" ++ (struct list "") ++ "\n}Model;\n"
        d_init = "Model model = {" ++ (initial list "") ++ "};\n"
    in d_struct ++ d_init


printThreads :: Int -> String
printThreads num =
    if num == -1 then ""
    else 
        let err = "int err" ++ show num ++ ";\n"
            th = "err"++ show num ++ "= pthread_create(&(tid["++show num ++"]), NULL, &thread"++show num++", NULL);\n"
            catch = "if (err"++ show num ++" != 0) \n printf(\"can't create thread :[%s]\", strerror(err"++show num ++"));\n" ++ "else\nprintf(\" Thread "++show num ++" created successfully\");\n"
        in err ++th++catch ++ printThreads (num-1)



generate_Main ::[SignalNode] -> String
generate_Main nodeL =
    let pthread = "pthread_t tid[" ++ show (length nodeL) ++ "];\n"
        main_frame contend = "int main(void)\n{\n" ++ contend ++ "sleep(5000);\nreturn 0;\n}\n"
        threds = printThreads ((length nodeL) -1)
    in pthread ++ main_frame threds
    
    

c_generator :: [Decl] -> SignalGraph -> String
c_generator astp (SG nodeL) = 
    let num_node = foldr (+) 0 (map countNum nodeL)
        -- threads_code = foldr (++) "\n" (map threadGen nodeL)
        list_signalNode = foldr (++) [] (map flattenST nodeL)
        clean_node = deleteR list_signalNode
        pair_node = zipNode_Index clean_node
        model_ = model_Gen clean_node
        threads_ = foldr (++) "" (map (\(a1,a2) -> threadGen a1 a2) pair_node) 
        head_ = "#include<stdio.h>\n#include<string.h>\n#include<pthread.h>\n#include<stdlib.h>\n#include<unistd.h>\n#include \"wiringPi.h\"\n\n"
        main_ = generate_Main clean_node
    in head_ ++ model_ ++ threads_ ++ main_

