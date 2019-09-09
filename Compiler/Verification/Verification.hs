module Verification.Verification where
import Verification.Antimirov
import Parser.AST 
import Debug.Trace


data VeriPair = VeriPair
 { 
    nameVer  :: Name,
    exprVer :: Expr, 
    preCond  :: Effect, 
    postCond  :: Effect
 } deriving (Show, Eq)

constructVeriPair :: [Decl] -> Decl -> [Decl] -> [Decl]
constructVeriPair astp eff acc = 
    let effname = getNameFromEffect eff
        helper [] acc_he = acc_he
        helper (x:xs) acc_he = 
            case x of
                Definition name _ _ -> 
                    if name == effname then [x] ++ acc_he 
                    else helper xs acc_he
                otherwise ->helper xs acc_he
    in helper astp acc
                
showVeriPair :: VeriPair -> String
showVeriPair pair = 
    case pair of 
        VeriPair n e pre post -> 
            "function " ++ n ++ "[final]" ++ printE  (normal pre) ++ " [post]" ++ printE  (normal post)


getNameFromEffect :: Decl -> String
getNameFromEffect eff = 
    case eff of 
        EFFECT name _ _ -> name
        otherwise -> "null" 
        
getPairs :: [Decl] -> [Decl] -> [VeriPair] -> [VeriPair]
getPairs [] [] acc = acc
getPairs (eff:effR) (def:defL) acc =
    case (eff, def) of 
        (EFFECT nameE pre post, Definition nameD pL expr ) ->
            getPairs  effR defL ([VeriPair {nameVer =nameE, exprVer = expr, preCond = pre, postCond =post}] ++ acc)
        otherwise -> getPairs  effR defL acc
        
filterPair :: [Decl] -> [VeriPair]
filterPair astp =
    let effectL = filter (\node -> 
                    case node of 
                        EFFECT _ _ _ -> True
                        otherwise -> False   
                        ) astp
        defL = foldr (constructVeriPair astp) [] effectL
        pairL = getPairs effectL defL []
    in pairL
    
accumulateFromExpr :: ([VeriPair], Effect) -> Expr -> Effect
accumulateFromExpr (pairL, currentS) expr = 
    let search [] name = Empty
        search (x:xs) name =
            case x of 
                VeriPair n e pre post -> if n == name then post else search xs name 
    in 
     case expr of 
        EFF event exprIn -> append (append currentS (Singleton event)) (accumulateFromExpr (pairL, currentS) exprIn)
        Lambda pl exprIn -> accumulateFromExpr (pairL, currentS) exprIn
        If ifThenList elseB -> 
            let execyteL = map (\(x,y) -> y) ifThenList
            in disjunction (
                --accumulateFromExpr currentS (head execyteL)
                foldr (\e a -> disjunction (accumulateFromExpr (pairL, currentS) e) a) Bottom execyteL
            ) (accumulateFromExpr (pairL, currentS) elseB )
        App name _ -> 
            --trace ("APP:" ++ printE (search pairL name) )
            search pairL name 
        Var name -> 
            --trace ("Var:" ++ printE (search pairL name) )
            search pairL name 
        otherwise -> Empty



accumulatePreCond :: [VeriPair] -> VeriPair -> VeriPair
accumulatePreCond pairL pair =
    case pair of
        VeriPair n e pre post ->
            VeriPair n e (accumulateFromExpr (pairL, pre) e) post

verification ::  [Decl] -> IO ()
verification astp = 
    let varPairs = filterPair astp
        finalPairs= map (accumulatePreCond varPairs) varPairs
        helper [] = print ""
        helper (x:xs) = 
            case x of 
                VeriPair n e final post -> do {
                    (pR final post (containment final post [])) ;
                    helper xs
                    } 


    in helper finalPairs

