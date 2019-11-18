module Parser.Expression where
import Parser.AST
import Parser.Pattern 
import Text.ParserCombinators.Parsec 
import Control.Applicative ((<*), (*>), (<$>), (<*>))

----------------------------------------
eStr :: Parser Expr-- Char
eStr = do
    fc    <- char '\"'
    mc  <- many $ oneOf anyChar
    ec   <- lexeme $ char '\"'
    return $ Str mc
  where anyChar = ['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9'] ++"_" ++ "-" ++ "." ++ " " 


----------------------------------------
decimalNumE :: Parser Expr
decimalNumE = do
    fc  <- lexeme $ many1 $ oneOf numChar
    mc  <- optionMaybe $ char '.'
    case mc of 
        Nothing -> return $ Int (read fc)
        Just dot -> do 
                ec <- lexeme $ many1 $ oneOf numChar
                return $ Float (read (fc++"."++ec))
  where numChar = ['0'..'9']

hexNumE :: Parser Expr
hexNumE = do
    fc  <- lexeme $ string "0X"
    rest  <- lexeme $ many1 $ oneOf numChar
    return $ Int (read $ fc ++ rest)
  where numChar = ['0'..'9']

decimalNum :: Parser Int
decimalNum = do
    fc  <- lexeme $ many1 $ oneOf numChar
    mc  <- optionMaybe $ char '.'
    case mc of 
        Nothing -> return $ (read fc)
        Just dot -> do 
                ec <- lexeme $ many1 $ oneOf numChar
                return $  (read (fc++"."++ec))
  where numChar = ['0'..'9']

eNum :: Parser Expr-- Int
eNum = try hexNumE <|> decimalNumE

----------------------------------------

eVar :: Parser Expr-- String
eVar = do
    fc <- lexeme_spa $ lowVar_Not_Key -- <|> lowVar_from_lib
    rest <- lexeme_spa $ optionMaybe $ many1 $expr
    case rest of 
        Nothing -> return $ Var fc
        Just rr -> return $ App fc rr

{--eNegate :: Parser Expr -- Expr
eNegate = do
    fc    <- lexeme $ char '-'
    mc    <- lexeme $ expr
    return $ Negate mc   --}  

eBinops :: Parser Expr-- String Expr Expr
eBinops = do
    fe    <- lexeme $ expr0
    mc    <- try  (lexeme $ string "==") <|> (lexeme $try $ string "++")  <|> (lexeme $ string "+") <|> (lexeme $ string "-") <|> (lexeme $ string "*") <|> (lexeme $ string "/") <|> (lexeme $ string "=") <|> (lexeme $ string ">") <|> (lexeme $ string "<") 
    ec    <- lexeme_spa $ expr
    return $ Binops mc fe ec  

----------------------------------------
elseifE :: Parser (Expr,Expr)
elseifE = do 
    elseif_word <- lexeme $ string "else"
    elseif_word <- lexeme $ string "if"
    elseif_ <- lexeme $ expr
    then_word <- lexeme $ string "then"
    then_ <- lexeme $ expr
    return $ (elseif_,then_)

eIf :: Parser Expr-- [(Expr, Expr)] Expr
eIf = do
    if_word <- try $ lexeme $ string "if"
    if_ <- lexeme $ expr
    then_word <- lexeme $ string "then"
    then_ <- lexeme $ expr
    
    manyelseif <- many $ try elseifE
    
    else_word <- lexeme $ string "else"
    else_ <- lexeme $ expr
    
    return $ If ([(if_,then_)]++manyelseif) else_
----------------------------------------
dataTypecell :: Parser (String,Expr)
dataTypecell = do 
    commo <- lexeme $ char ',' 
    c <- lexeme $ lowVar
    eq <- lexeme $ char '=' 
    ex <- lexeme $ expr
    return (c,ex)

    {--
eRecord :: String -> Parser Expr
eRecord fst= do 
    eq <- lexeme $ char '=' 
    ex <- lexeme $ expr
    mc <- try $ lexeme $ try $ many dataTypecell 
    ec <- try $ lexeme $ char '}'
    return $ Record ([(fst,ex)]++mc)

eUpdate :: String -> Parser Expr
eUpdate fst= do 
    -- fc <- lexeme $ lowVar
    eq <- lexeme $ char '|' 
    mc <- try $ lexeme $ try $ sepBy dataTypecell (lexeme $ char ',') 
    ec <- try $ lexeme $ char '}'
    return $ Update fst mc


eRecord_Update :: Parser Expr -- [(String, Expr)]
eRecord_Update = do 
    fc <- try $ lexeme $ char '{' 
    fst <- lexeme $ lowVar
    ec <- lexeme $ (eRecord fst) <|> (eUpdate fst) 
    return ec
--}

eList :: Parser Expr-- [Expr]
eList = do 
    lb <- lexeme $ char '[' 
    content <- try $ sepBy expr (lexeme $ char ',') 
    rb <- lexeme_spa $ char ']' 
    return $ List content


eLambda :: Parser Expr-- [Pattern] Expr
eLambda = do 
    fc <- lexeme $ char '\\'
    mc <- lexeme $ many $ pattern
    eq <- lexeme $ string "->"
    ec <- lexeme $ expr
    return $ Lambda mc ec

eLet :: Parser Expr-- [Def] Expr
eLet = do
    let_ <- try $ lexeme $ string "let"
    fun_<- lexeme $ many1 defination
    in_ <- lexeme $ string "in"
    expr_<- lexeme $ expr 
    -- case_body <- lexeme $ many1 $ try case_bodyE
    return $ Let fun_ expr_--(Var "aa")--case_object case_body

case_bodyE :: Parser (Pattern,Expr)
case_bodyE = do 
    fp <- lexeme $ pattern <|> pVar1
    arrowc <- lexeme $ string "->"
    ec <- lexeme $ expr 
    return $ (fp , ec)

eParent :: Parser Expr 
eParent = do 
    lb <- lexeme $ char '(' 
    first <- lexeme $ expr
    rb <- lexeme_spa $ char ')' 
    return $  first

   
eBoolT :: Parser Bool
eBoolT = do 
    re <- (string "True")
    return $ True

eBoolF :: Parser Bool
eBoolF = do 
    re <- (string "False")
    return $ False

eBool :: Parser Expr
eBool = do  
    fp <- lexeme $ eBoolF <|> eBoolT
    return $ Boolean fp

eFold :: Parser Expr
eFold = do 
    kw <- lexeme $ string "fold"
    fun <- lexeme $ expr
    acc <- lexeme $ expr0
    input <- lexeme $  expr
    return $ Fold fun acc input

eEffect :: Parser Expr
eEffect = do 
    kw <- lexeme $ string "effect"
    event <- lexeme $ str
    ex <- lexeme $  expr
    return $ EFF event ex

eLift:: Parser Expr
eLift = do 
    kw <- lexeme $ try $ string "lift"
    fun <- lexeme $ expr
    input <- lexeme $ expr
    return $ Lift fun input

eSync:: Parser Expr
eSync = do 
    kw <- lexeme $ string "sync"
    input <- lexeme $ expr
    return $ Sync input

eSignal:: Parser Expr
eSignal = do 
    kw <- lexeme $ string "Signal"
    input <- lexeme $ expr
    return $ Signal input

ePrior:: Parser Expr
ePrior = do 
    kw <- lexeme $ string "prior"
    level <- lexeme $ decimalNum
    input <- lexeme $ expr
    return $ Prior level input

eLift2:: Parser Expr
eLift2 = do 
    kw <- lexeme $  try $ string "lift_2"
    fun <- lexeme $ expr
    input1 <- lexeme $ expr
    input2 <- lexeme $ expr
    return $ Lift2 fun ([input1] ++ [input2])

eLift3:: Parser Expr
eLift3 = do 
    kw <- lexeme $ try $ string "lift_3"
    fun <- lexeme $ expr
    input1 <- lexeme $ expr
    input2 <- lexeme $ expr
    input3 <- lexeme $ expr
    return $ Lift3 fun ([input1] ++ [input2] ++ [input3])
----------------------------------------
expr0 :: Parser Expr
expr0 = try eStr <|> eNum <|> eVar -- <|> eNegate <|> eList <|> eTag


eLiftn :: Parser Expr
eLiftn = try eLift <|> eLift2 <|> eLift3

expr :: Parser Expr
expr = try eBinops <|> expr0  <|> eParent <|> eFold  <|>eLiftn  <|>eLambda <|> eIf <|> eLet <|> eBool <|> eList <|> eSync <|> eSignal <|> ePrior <|> eEffect
-- <|>eVar <|> eStr <|> eNum  --  <|> eRecord_Update <|> eTupple  <|> eNegate 
-- eTag<|>eCase<|>

-- expression :: Parser Expr
-- expression = try eIf <|> eLet <|> eCase<|> eLambda
--exprs :: Parser [Expr]
--exprs = spaces *> many (lexeme $ expr)
--------------------

defination :: Parser Def --(String) [Pattern] Expr
defination = do 
    fc <- lexeme $ lowVar_Not_Key
    mc <- lexeme $ many $ pattern
    eq <- lexeme $ char '='
    ec <- lexeme $ expr
    return $ Define fc mc ec