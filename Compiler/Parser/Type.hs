module Parser.Type where
import Parser.AST
import Text.ParserCombinators.Parsec 
import Control.Applicative ((<*), (*>), (<$>), (<*>))



tBtoBfun_left:: Parser Type 
tBtoBfun_left = do 
    _ <- lexeme_spa $ string "->"
    ec <- lexeme_spa $ type_
    return ec

functionType :: Parser Type
functionType = do
    fir <- try $ lexeme_spa $ basicType <|> signalType
    snd <- try $ tBtoBfun_left
    return $ (TFun fir snd)


int_type :: Parser Type 
int_type = do 
    _ <- string "Int"
    return $ TInt 
    --_ <- char 'I'
    --ne <- try  (lexeme_spa $ string "nt")  <|> (lexeme_spa $ string "O")
    --case ne of 
        --"nt" ->  do return $ TInt 
        -- "O" -> do return $ TIO

bool_type :: Parser Type 
bool_type = do 
    _ <- string "Bool"
    return $ TBool 
    
str_type :: Parser Type 
str_type = do 
    _ <- string "String"
    return $ TString 

unit_type :: Parser Type 
unit_type = do 
    _ <- string "()"
    return $ TUnit 

basicType :: Parser Type 
basicType = do
    basic <- try str_type <|>int_type  <|> bool_type <|> unit_type 
    return basic

signalType :: Parser Type 
signalType = do
    _ <- lexeme $ string "Signal"
    t <- basicType
    return $ TSignal t

type_ :: Parser Type 
type_ = try functionType <|> basicType <|> signalType   --tLambda <|> list  <|>tuple <|> singleton


types_ :: Parser [Type]
types_ = spaces *> many (lexeme_spa $ type_)

