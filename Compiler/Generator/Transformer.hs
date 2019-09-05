module Generator.Transformer where
import Generator.JSAST
import Parser.AST


lib_name = "_lib"

ext_rec_by_name :: [(String, Expr)] -> String -> Expr
ext_rec_by_name (x:xs) name= 
    case x of (a,b) -> if a == name then b else ext_rec_by_name xs name

trans_expr :: Expr -> String
trans_expr (Str p) = p
trans_expr (Int p) = show p
trans_expr (Boolean p) = p
trans_expr (Var p) = p 

trans_op :: String -> JSBinOp
trans_op "+" = Plus
trans_op "-" = Minus
trans_op "*" = Times
trans_op "/" = Divide
trans_op "<" = Lt
trans_op "<=" = Le
trans_op ">" = Gt
trans_op ">=" = Ge
trans_op "&&" = Andand
trans_op "==" = EqEq

trans_expr_to_JS :: Expr -> JSExpr
trans_expr_to_JS (Str p) =  JSString p
trans_expr_to_JS (Int p) =  JSInt $ show p
trans_expr_to_JS (Var p) =  JSId p
trans_expr_to_JS (Tag s e) = 
    let helper [] temp = temp
        helper (x:xs) temp = helper xs (temp ++ [(trans_expr_to_JS x)])
    in JSTagList ([(JSString s)] ++ (helper e []) )
trans_expr_to_JS (Record e) = 
    let helper [] temp = temp
        helper (x:xs) temp = 
          case x of 
            (a, b) -> helper xs (temp ++ [(a,(trans_expr_to_JS b))])
    in JSRecord (helper e [])

trans_expr_to_JS (Binops a b c) = 
    JSExprBinary (trans_expr_to_JS b) (trans_op a) (trans_expr_to_JS c)

trans_expr_to_JS (Call "second" a) = 
    JSIndex (trans_expr_to_JS (head a)) (JSInt "1")
trans_expr_to_JS (Call "first" a) = 
    JSIndex (trans_expr_to_JS (head a)) (JSInt "0")

trans_expr_to_JS (Tupple a b _) = 
    JSList [(trans_expr_to_JS a), (trans_expr_to_JS b)]


trans_expr_to_JS_state :: Expr -> JSState
trans_expr_to_JS_state (Case a b) = 
    let lsh = JSIndex (trans_expr_to_JS a) (JSInt "0")
        rhs = JSIndex (trans_expr_to_JS a) (JSInt "1")
        helper (Case a [x]) = 
            case x of 
                (PVar "otherwise" , e )-> JSReturn (Just (trans_expr_to_JS e)) Semi
                -- otherwise -> Empty
        helper (Case a (x:xs)) = 
            case x of   --(pattern,expr)
                (PTuple t1 t2 _ ,e)-> JSIfElse (JSExprBinary (JSExprBinary lsh EqEq (trans_pattern_to_JSExpr t1)) (Andand) (JSExprBinary rhs EqEq (trans_pattern_to_JSExpr t2))) (JSReturn (Just (trans_expr_to_JS e)) Semi) (helper (Case a xs)) 
                (PCtor c1 c2, e) ->  JSIfElse (JSExprBinary lsh EqEq (JSId c1)) (trans_expr_to_JS_state e) (helper (Case a xs)) 
                otherwise -> Empty   
    in helper (Case a b)

trans_expr_to_JS_state (If a b) = --[(Expr, Expr)] Expr 
    let this_one = head a 
        cond = case this_one of 
                (e1,e2) -> e1
        res = case this_one of 
                (e1,e2) -> e2
    in if (length a) == 1 then JSIfElse (trans_expr_to_JS cond) (JSReturn ( Just (trans_expr_to_JS res)) Semi ) ((JSReturn (Just (trans_expr_to_JS b)) Semi))--
       else JSIfElse (trans_expr_to_JS cond) (JSReturn ( Just (trans_expr_to_JS res)) Semi ) (trans_expr_to_JS_state (If (tail a) (b)))
trans_expr_to_JS_state (Tupple a b _) = 
    JSStateList [(trans_expr_to_JS a), (trans_expr_to_JS b)]



    
trans_pattern_to_JSExpr :: Pattern  -> JSExpr
trans_pattern_to_JSExpr (PStr s) = JSString s

trans_pattern_to_JS :: Pattern  -> String
trans_pattern_to_JS p = 
    case p of 
        PStr p  -> p
        PInt p  -> show p
        PVar p  -> p
        PTuple p1 p2 _ -> ("(" ++ (trans_pattern_to_JS p1) ++ ", " ++ (trans_pattern_to_JS p2) ++ ")" )

trans_state_to_JS :: Decl -> JSState
trans_state_to_JS (Definition c d e) = 
    JSFunction c (map trans_pattern_to_JS d) (trans_expr_to_JS_state e)


view_sen_list :: Expr -> [JSState]
view_sen_list (List li) = 
    let helper (Call a [(Call b [(Tag c [(Var name)])])]) = 
            JSCallDot (JSId name) (JSMemberExpr(JSId "fetch") [(JSFunctionExpression ("") [("err"),("num")] [(JSMemberExpr (JSId "update") [(JSList [(JSId "num.type"),(JSId "num.value")]),(JSId "model")])] )]) Semi
    in map helper li 


view_dev_list :: Expr -> [JSState]
view_dev_list (List li) = 
    let helper (Call a [(Call b [(Call c [(Var name)])])]) = 
            JSCallDot (JSId name) (JSMemberExpr (JSId "writeSync") [(JSMemberExpr (JSId b) [JSId c])]) Semi
    in map helper li

update_body :: Expr -> JSState
update_body (Case a b) = trans_expr_to_JS_state (Case a b)
------------------------------------------------------------------
transformer :: [Decl] -> [JSState] -> JSAST
transformer [] temp  = JSAstProgram temp 
transformer ((Annotation a (TTypeQual "Device" [])):(Definition c d (Record e)):xs) temp = 
    let pin   =  trans_expr $ ext_rec_by_name e "d_pin"
        lib   =  trans_expr $ ext_rec_by_name e "d_lib"
        func  =  trans_expr $ ext_rec_by_name e "d_func"
        dir   =  trans_expr $ ext_rec_by_name e "d_dir"
        state1 = JSVariable [(JSVarInitExpr (JSId (c++lib_name)) (JSMemberDot (JSMemberExpr (JSId "require") [(JSString lib)] ) (JSId func)))] Semi
        state2 = JSVariable [(JSVarInitExpr (JSId c) (JSMemberNew (JSId (c++lib_name)) [(JSInt pin),(JSString dir)] ))] Semi
    in transformer xs (temp++ [state1] ++  [state2]) 

transformer ((Annotation a (TTypeQual "Sensor" [])):(Definition c d (Record e)):xs) temp = 
    let lib   =  trans_expr $ ext_rec_by_name e "s_lib"
        constFun =  trans_expr $ ext_rec_by_name e "s_constFun"
        type_  =  trans_expr $ ext_rec_by_name e "s_type"
        address   =  trans_expr $ ext_rec_by_name e "s_address"
        desc   =  trans_expr $ ext_rec_by_name e "s_desc"

        state1 = JSVariable [(JSVarInitExpr (JSId (c++lib_name)) (JSMemberExpr (JSId "require") [(JSString lib)]))] Semi
        
        state2 = JSVariable [(JSVarInitExpr (JSId c) (JSMemberNew (JSMemberDot (JSId (c++lib_name)) (JSId constFun)) [(JSRecord [(("type"),(JSString type_)),(("address"),(JSInt address))]),(JSString desc)] ))] Semi
    in transformer xs (temp++ [state1] ++  [state2]) 

transformer ((Annotation a b):(Definition "model" d e):xs) temp = 
    let model_list = 
          case e of 
            Int i -> [JSInt (show i)]
            Tupple t1 t2 _ -> 
                let helper pp = 
                        case pp of JSTagList li -> li
                    taglist1 = head (helper (trans_expr_to_JS t1))
                    taglist2 = head (helper (trans_expr_to_JS t2))
                in [taglist1,taglist2] 
        statement = JSVariable [(JSVarInitExpr (JSId "model") (JSList model_list))] Semi
    in transformer xs (temp ++ [statement])


transformer ((Definition "view" d (Call "iot" sen_dev)):xs) temp =
    let sensor_list = (sen_dev !! 0)
        device_list = (sen_dev !! 1)
        statement = JSWhile (JSBool "true") (JSStateBlock ((view_sen_list sensor_list)++(view_dev_list device_list)))
    in transformer xs (temp++[statement]) 


transformer ((Definition "update" d e):xs) temp =
    let params [] temp = temp
        params (x:xs) temp = params xs (temp ++ [trans_pattern_to_JS x])
        statement = JSFunction ("update") (params d []) (update_body e)
    in transformer xs (temp++[statement]) 


transformer ((Definition c d e):xs) temp = 
    if c == "iot_main" 
        then transformer xs temp
        else let body_expr = trans_state_to_JS (Definition c d e)
             in  transformer xs (temp++[body_expr]) 

transformer (x:xs) temp = transformer xs (temp++[Empty]) 

