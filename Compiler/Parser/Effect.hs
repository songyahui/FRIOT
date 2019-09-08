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


dot_left:: Parser Effect 
dot_left = do 
    _ <-  lexeme_spa $ char '.'
    ec <-  lexeme_spa $ basic_with_or
    return ec

dot :: Parser Effect
dot = do
    fir <- try $ lexeme_spa $ singleton
    snd <- try $ dot_left
    return $ (Dot fir snd)

orE_left:: Parser Effect 
orE_left = do 
    _ <-  lexeme_spa $ char '+'
    ec <-  lexeme_spa $ basic_with_dot
    return ec

orE :: Parser Effect
orE = do
    fir <- try $ lexeme_spa $ singleton
    snd <- try $ orE_left
    return $ (Dot fir snd)


{-
and 
star 
neg
-}
parentEff:: Parser Effect 
parentEff = do 
    lb <- lexeme $ char '(' 
    first <- lexeme $ effect_
    rb <- lexeme_spa $ char ')' 
    return $ first

basic :: Parser Effect 
basic = try singleton <|> botton <|> empty 

basic_with_dot :: Parser Effect 
basic_with_dot = try dot <|> basic 

basic_with_or :: Parser Effect 
basic_with_or = try orE <|> basic 


effect_ :: Parser Effect 
effect_ = try dot <|> orE  <|> parentEff  <|>   basic