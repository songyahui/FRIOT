module Generator.Generator where
import Parser.AST



gen_state :: JSState -> String 
gen_state (JSFunction a b c) = 
    let helper [x] temp = (temp ++ x)
        helper (x:xs) temp = helper xs (temp ++ x ++ ", ")
        args = 
            case b of 
                [] -> ""
                otherwisw -> "(" ++ (helper b "") ++ ")"
    in "function " ++ a ++ args ++ "{\n" ++ (gen_state c) ++ "}"
gen_state (JSReturn a b) = 
    case a of 
        Nothing -> "return;"
        Just con -> "return " ++ (gen_expr con) ++ ";\n"

gen_state (JSIfElse a b c) = 
    "if " ++ (gen_expr a) ++ "{\n " ++ (gen_state b) ++ "}\n else " ++ (gen_state c) 

gen_state (JSWhile a b) = 
    "while (" ++ (gen_expr a) ++ "){\n " ++ (gen_state b) ++ " var sleep = require('system-sleep');\n sleep(5000);\n }"

gen_state (JSCallDot a b c) = 
    (gen_expr a) ++"." ++ (gen_expr b) ++ ";\n"

gen_state (JSStateBlock a) =
    let helper [] temp = temp
        helper (x:xs) temp = helper xs (temp ++ (gen_state x)) 
    in helper a ""

gen_state (JSStateList e) =  
    let helper [] temp = temp
        helper [x] temp = temp ++ (gen_expr x)
        helper (x:xs) temp = helper xs (temp ++ (gen_expr x) ++ ", ")
    in "[ " ++ (helper e "") ++ " ]"

gen_state s = "statemnte"

gen_expr :: JSExpr -> String 
gen_expr Empty1 =  ""
gen_expr (JSId p) =  p
gen_expr (JSInt p) = p 
gen_expr (JSBool p) =  p
gen_expr (JSString p) =  "\'" ++ p ++ "\'"
gen_expr (JSList e) =  
    let helper [] temp = temp
        helper [x] temp = temp ++ (gen_expr x)
        helper (x:xs) temp = helper xs (temp ++ (gen_expr x) ++ ", ")
    in "[ " ++ (helper e "") ++ " ]"

gen_expr (JSVarInitExpr a b) = (gen_expr a) ++ " = " ++ (gen_expr b)
gen_expr (JSRecord a) = 
    let helper [x] temp =  case x of (p1,p2) -> temp ++ p1 ++ " : " ++ (gen_expr p2)
        helper (x:xs) temp = 
            case x of (p1,p2) -> helper xs (temp ++ p1 ++ " : " ++ (gen_expr p2) ++ ", ")
    in "{\n " ++ (helper a "" ) ++ " }"   
gen_expr (JSMemberDot a b) = (gen_expr a) ++ "." ++ (gen_expr b) -- firstpart.name
gen_expr (JSMemberExpr a b) = 
    let helper [x] temp = temp ++ (gen_expr x)
        helper (x:xs) temp = (helper xs ((gen_expr x) ++ ", "))
    in 
        case a of 
            JSId "update" -> "model = " ++ (gen_expr a) ++ "( " ++ (helper b "") ++ " )"
            otherwisw -> (gen_expr a) ++ "( " ++ (helper b "") ++ " )"  --JSExpr [JSExpr] -- expr(args)
gen_expr (JSMemberNew a b) = 
    let helper [x] temp = temp ++  (gen_expr x)
        helper (x:xs) temp = helper xs ((gen_expr x) ++ ", ")
    in "new " ++ (gen_expr a) ++ "(" ++ (helper b "") ++ ")"  --JSExpr [JSExpr] -- new, name(args)
gen_expr (JSExprBinary a b c) = 
    let lhs = (gen_expr a)
        rhs = (gen_expr c)
        op = case b of 
                Divide -> " / " -- /
                Eq     -> " = " -- =
                Ge     -> " >= "-- >=
                Gt     -> " > "-- >
                Le     -> " <= " -- <=
                Lt     -> " < " -- <
                Minus  -> " - " -- -
                Neq    -> " != " -- =
                Plus   -> " + "-- +
                Times  -> " * " -- *
                Andand -> " && " -- && 
                EqEq   -> " == " -- ==
    in "(" ++ lhs ++ op ++ rhs ++ ")"
gen_expr (JSFunctionExpression a b c) = 
    let args [] temp = temp
        args [x] temp = temp ++ x 
        args (x:xs) temp = args xs (temp ++ x ++ ", ")
        state [] temp = temp
        state (x:xs) temp = state xs (temp ++ (gen_expr x) ++ ";")
    in "function " ++ a ++ "(" ++ (args b "") ++ ")" ++ "{\n"++ (state c "") ++ "}"
gen_expr (JSIndex a b) = (gen_expr a) ++ "[" ++ (gen_expr b)++ "]"
gen_expr (JSTagList e) = (gen_expr (head e))
    -- JSExpr JSBinOp JSExpr -- lhs, op, rhs

------------------------------------------------------------------
generator_all :: [JSState] -> String -> String
generator_all [] temp = temp
generator_all ((JSVariable a b):xs) temp = 
    let helper [x] temp = temp ++ (gen_expr x)
        helper (x:xs) temp = helper xs ((gen_expr x) ++ ",")
    in generator_all xs (temp ++ "var " ++ (helper a "") ++ ";\n\n")
generator_all ((JSFunction a b c):xs) temp = 
    let function = gen_state (JSFunction a b c)
    in generator_all xs (temp ++ function ++ "\n\n")

generator_all ((JSIfElse a b c):xs) temp =
    let ifelse = gen_state (JSIfElse a b c)
    in generator_all xs (temp ++ ifelse ++ "\n\n")

generator_all ((JSWhile a b):xs) temp =
    let while_loop = gen_state (JSWhile a b)
    in generator_all xs (temp ++ while_loop ++ "\n\n")

generator_all ((JSCallDot a b c):xs) temp =
    let call_dot = gen_state (JSCallDot a b c)
    in generator_all xs (temp ++ call_dot ++ "\n\n")

generator_all ((JSStateBlock a):xs) temp =
    let helper [] temp = ""
        helper (x:xs) temp = helper xs (temp ++ (gen_state x)) 
    in generator_all xs (temp ++ (helper a "") ++ "\n\n")
   

generator_all (x:xs) temp = generator_all xs temp


generator :: JSAST -> String
generator ast = 
    case ast of 
        JSAstProgram list -> generator_all list ""
