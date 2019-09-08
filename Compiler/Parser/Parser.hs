module Parser.Parser where
import Parser.AST 
import Parser.Type 
import Parser.Declaration
import Text.ParserCombinators.Parsec 
import Control.Applicative ((<*), (*>), (<$>), (<*>))


build_AST :: SourceName -> String -> Either ParseError [Decl]
build_AST = runParser declarations ()

test_type :: SourceName -> String -> Either ParseError [Type]
test_type = runParser types_ ()
----------------------------------
---- filt all the comments -------
---  clearComments 
---  delete_till_return
----------------------------------
clearComments:: String -> String -> String --(char r[],int pProject)
clearComments [] oupStr =  oupStr
clearComments (x:xs) oupStr
    | x == '{' && ((head xs) == '-') && ((head $ tail xs) == '@') =  clearComments (tail xs) (oupStr)
    | x == '$' = clearComments xs oupStr
    | x == '!' && ((head xs) == '!') = clearComments (tail xs) oupStr
    | x == '-' && ((head xs) == '-') = clearComments (clearComments_oneline (tail xs)) oupStr
    | x == '{' && ((head xs) == '-') = clearComments (clearComments_muilty (tail xs)) oupStr
    | otherwise = clearComments xs (oupStr ++ [x])
   
clearComments_muilty :: String -> String
clearComments_muilty [] = []
clearComments_muilty (x:xs) 
    | x == '-' &&  ((head xs) == '}') = tail xs 
    | otherwise = clearComments_muilty xs

clearComments_oneline:: String -> String
clearComments_oneline [] = []
clearComments_oneline (x:xs) 
    | x == '\n' = (x:xs) 
    | otherwise = clearComments_oneline xs

-----------------------------------------------------

parse :: String -> String -> Either ParseError [Decl]
parse fileName inpStr = 
    let inpStr_no_comments = clearComments inpStr "" 
    in build_AST fileName inpStr_no_comments 