module Parser.Effect where
import Parser.AST
import Text.ParserCombinators.Parsec 
import Control.Applicative ((<*), (*>), (<$>), (<*>))
import Antimirov.Antimirov

botton :: Parser Effect 
botton = do 
    no <- lexeme $ char '_'
    return $ Bottom

empty :: Parser Effect 
empty = do 
    no <- lexeme $ string "emp"
    return $ Empty

singleton :: Parser Effect 
singleton = do 
    no <- try $ lexeme $ str
    return $ Singleton no

connect :: Parser Effect
connect = do
    fir <- try $ lexeme_spa $ parentEff
    sign <- try $ lexeme_spa $ optionMaybe $ (char '.' <|> char '+' <|> char '&' <|> char '*')
    case sign of 
        Nothing -> return $ fir
        Just '*' -> return $ Star fir
        Just '.' -> do 
            snd <-  try $ lexeme_spa $ parentEff
            return $  Dot fir snd 
        Just '+' -> do 
            snd <-  try $ lexeme_spa $ parentEff
            return $  OR fir snd 
        Just '&' -> do 
            snd <-  try $ lexeme_spa $ parentEff
            return $  And fir snd 
    

orE_left:: Parser Effect 
orE_left = do 
    _ <-  try $ lexeme_spa $ char '+'
    ec <-  try $ lexeme_spa $ parentEff
    return ec

orE :: Parser Effect
orE = do
    fir <- try $ lexeme_spa $ parentEff
    snd <- try $ orE_left
    return $ (And fir snd)


{-
and 

neg
-}

star:: Parser Effect 
star = do 
    fir <- try $ lexeme $ parentEff
    sign <- try $ lexeme $ string "^*"
    return $ Star fir

parentEff:: Parser Effect 
parentEff = do 
    lb <- try $ lexeme $ char '(' 
    first <- try $ lexeme $ effect_
    rb <- try $ lexeme_spa $ char ')' 
    return $ first

basic :: Parser Effect 
basic = try  singleton <|> botton <|> empty 


effect_ :: Parser Effect 
effect_ = try  connect  <|>  basic 