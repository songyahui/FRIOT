module Parser.Pattern where
import Parser.AST
import Text.ParserCombinators.Parsec 
import Control.Applicative ((<*), (*>), (<$>), (<*>))


pAnything :: Parser Pattern
pAnything = do 
    underscore <- lexeme $ char '_'
    return $ PAnything

pStr :: Parser Pattern--String
pStr = do
    fc    <- char '\"'
    mc  <- many $ oneOf anyChar
    ec   <- lexeme $ char '\"'
    return $ PStr mc
  where anyChar = ['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9'] ++"_" ++ "-" ++ "." ++ " " 


----------------------------------------
decimalNumP :: Parser Pattern
decimalNumP = do
    fc  <- lexeme $ many1 $ oneOf numChar
    return $ PInt (read fc)
  where numChar = ['0'..'9']

hexNumP :: Parser Pattern
hexNumP = do
    fc  <- lexeme $ string "0X"
    rest  <- lexeme $ many1 $ oneOf numChar
    return $ PInt (read $ fc ++ rest)
  where numChar = ['0'..'9']


pInt :: Parser Pattern--Int
pInt = try hexNumP <|> decimalNumP


pVar :: Parser Pattern--String
pVar = do
    fc  <- try $ lexeme $ lowVar_Not_Key
    return $ PVar fc
    
pVar1 :: Parser Pattern--String
pVar1 = do
    fc  <- try $ lexeme $ lowVar
    return $ PVar fc

{--
pCons :: Parser Pattern--Pattern Pattern
pCons = do 
    fc <- try $ lexeme $ pVar
    mc <- try $ lexeme $ string "::"
    ec <- try $ lexeme $ pattern
    return $ PCons fc ec

list :: Parser Pattern--[Pattern]    -- ?
list = do 
    lb <- lexeme $ char '[' 
    content <- try $ sepBy pattern (lexeme $ char ',') 
    rb <- lexeme $ char ']' 
    return $ PList content
--}

ptuple_left:: Parser Pattern 
ptuple_left = do 
    _ <- lexeme_spa $ char ','
    ec <- lexeme_spa $ pattern
    return ec

tuplePattern :: Parser Pattern
tuplePattern = do
    fir <- try $ lexeme_spa $ pattern
    snd <- try $ many1 ptuple_left
    return $ (Ptuple ([fir] ++ snd))


ptuple :: Parser Pattern-- Pattern Pattern [Patter
ptuple = do 
    lb <- lexeme $ char '(' 
    first <- lexeme $ tuplePattern
    rb <- lexeme_spa $ char ')' 
    return $  first


    


--     first <- lexeme $ pattern0
--     cb1 <- lexeme $ char ',' 
--     second <- lexeme $ pattern0
--     cb2 <- optionMaybe $ lexeme $ char ',' 
--     case cb2 of 
--         Nothing -> do 
--             rb1 <- lexeme $ char ')' 
--             return $ PTuple first second []
--         Just dot -> do 
--             rest <- try $ sepBy pattern0 (lexeme $ char ',') 
--             rb2 <- lexeme $ char ')' 
--             return $ PTuple first second rest

{--pPCtor:: Parser Pattern
pPCtor = do 
    fc <- lexeme $ uppVar
    cd <- lexeme $ many pattern
    return $ PCtor fc cd
    --}


termHelp:: Parser Pattern
termHelp = try pAnything <|>  pVar <|>  pInt <|> pStr -- <|> pPCtor

{--

record :: Parser Pattern
record = do 
    lb <- lexeme $ char '{' 
    content <- sepBy lowVar (lexeme $ char ',') 
    rb <- lexeme $ char '}' 
    return $ PRecord content
--}

pattern_expr :: Parser Pattern
pattern_expr = do 
    fc <- lexeme $ name
    return $ PStr fc


pattern :: Parser Pattern
pattern = try ptuple <|>termHelp --record <|> list


--patterns :: Parser [Pattern]
--patterns = spaces *> many (lexeme $ pattern)