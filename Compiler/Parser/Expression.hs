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

eInt :: Parser Expr-- Int
eInt = try hexNumE <|> decimalNumE

----------------------------------------

eVar :: Parser Expr-- String
eVar = do
    fc <- lexeme_spa $ lowVar_Not_Key <|> lowVar_from_lib
    rest <- lexeme_spa $ optionMaybe $ many1 $expr
    case rest of 
        Nothing -> return $ Var fc
        Just rr -> return $ Call fc rr

eNegate :: Parser Expr -- Expr
eNegate = do
    fc    <- lexeme $ char '-'
    mc    <- lexeme $ expr
    return $ Negate mc     

eBinops :: Parser Expr-- String Expr Expr
eBinops = do
    fe    <- lexeme $ expr0
    mc    <- try  (lexeme $ string "==")  <|> (lexeme $ string "+") <|> (lexeme $ string "-") <|> (lexeme $ string "*") <|> (lexeme $ string "/") <|> (lexeme $ string "=") <|> (lexeme $ string ">") <|> (lexeme $ string "<") 
    ec    <- lexeme $ expr
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

eCase :: Parser Expr-- Expr [(Pattern, Expr)]
eCase = do
    casec <- try $  lexeme $ string "case"
    case_object<- lexeme $ expr
    thenc <- lexeme $ string "of"
    case_body <- lexeme $ many1 $ try case_bodyE
    return $ Case case_object case_body

eTupple :: Parser Expr --Expr Expr [Expr]
eTupple = do 
    lb <- lexeme $ char '(' 
    first <- lexeme $ expr

    cb1 <- optionMaybe $ lexeme $ char ',' 
    case cb1 of 
        Nothing -> do 
            rbb <- lexeme_spa $ char ')' 
            return $ Parent first
        Just kjnkn -> do 
            second <- lexeme $ expr0
            cb2 <- optionMaybe $ lexeme $ char ',' 
            case cb2 of 
                Nothing -> do 
                    rb1 <- lexeme_spa $ char ')' 
                    return $ Tupple first second []
                Just dot -> do 
                    rest <- try $ sepBy expr0 (lexeme $ char ',') 
                    rb2 <- lexeme_spa $ char ')' 
                    return $ Tupple first second rest

lowVar_from_lib :: Parser String
lowVar_from_lib = do 
  Tag fp mc <-  lexeme $ eTag
  dot <- char '.'
  lc <- lowVar_Not_Key
  return $ (fp ++ "." ++ lc)

eTag ::Parser Expr
eTag = do 
    fp <- try $ lexeme $ uppVar_Not_Key
    dot <- optionMaybe $ char '.'

    case dot of 
        Nothing -> do
            mc <- lexeme $ many expr
            return $ Tag fp mc
        Just a -> do 
            mc <- eVar
            case mc of 
                Var fc -> do return $ Var (fp ++"."++ fc)
                Call fc rr -> do return $ Call (fp ++"."++ fc) rr 
        
eBool :: Parser Expr
eBool = do  
    fp <- lexeme $ (string "False") <|> (string "True")
    return $ Boolean fp

----------------------------------------
expr0 :: Parser Expr
expr0 = try eStr <|> eInt <|> eVar <|> eNegate <|> eList <|> eTag

-- expr :: Parser Expr
-- expr = try eBinops <|>eIf <|> eCase <|> eLet <|> eTupple <|> eRecord_Update <|> eLambda <|> expr0

expr :: Parser Expr
expr = try  eBinops <|> eTag<|>eCase<|>eIf <|> eLet <|> eBool <|>eVar <|> eStr <|> eInt <|> eList  <|> eRecord_Update <|> eTupple  <|> eNegate <|> eLambda


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