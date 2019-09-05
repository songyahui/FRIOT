module Parser.Type where
import Parser.AST
import Text.ParserCombinators.Parsec 
import Control.Applicative ((<*), (*>), (<$>), (<*>))

tLambda_left:: Parser Type 
tLambda_left = do 
    _ <- lexeme_spa $ string "->"
    ec <- lexeme_spa $ (list <|>tuple <|> basicType)
    return ec


tLambda :: Parser Type 
tLambda = do 
    fir <- try $ (list <|>tuple  <|>singleton)
    snd <- try $ many1 tLambda_left
    return $ TLambda $ [fir] ++snd

list :: Parser Type -- [(String, Type)] 
list = do 
    _ <- lexeme $ char '[' 
    mc <- lexeme $ type_
    _ <- lexeme_spa $ char ']'
    return $ TList mc

tuple:: Parser Type -- Type Type [Type] --8
tuple = do 
    _ <- lexeme $ char '(' 
    first <- lexeme $ type_
    _ <- lexeme $ char ',' 
    second <- lexeme $ type_
    cb2 <- optionMaybe $ lexeme $ char ',' 
    case cb2 of 
        Nothing -> do 
            _ <- lexeme_spa $ char ')' 
            return $ TTuple first second []
        Just _ -> do 
            rest <- try $ sepBy type_ (lexeme $ char ',') 
            _ <- lexeme_spa $ char ')' 
            return $ TTuple first second rest

variable :: Parser Type -- String 
variable = do 
    fc  <- name
    return $ TVar fc

int_io_type :: Parser Type 
int_io_type = do 
    _ <- char 'I'
    ne <- try  (lexeme_spa $ string "nt")  <|> (lexeme_spa $ string "O")
    case ne of 
        "nt" ->  do return $ TInt 
        "O" -> do return $ TIO
    --string "Int"
    
{--
io_type :: Parser Type 
io_type = do 
    _ <- string "IO"
    return $ TIO
--}


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
    basic <- try str_type <|>int_io_type  <|> bool_type <|> unit_type
    return basic

singleton :: Parser Type 
singleton = do
    li <- try $ many1 $ lexeme_spa (basicType <|> variable)
    case li of 
        [x] -> return x
        _ -> return $ TApp li

    

type_ :: Parser Type 
type_ = try  tLambda <|> list  <|>tuple <|> singleton


types_ :: Parser [Type]
types_ = spaces *> many (lexeme_spa $ type_)

