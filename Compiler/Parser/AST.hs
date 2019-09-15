module Parser.AST where
import Data.Char
import Text.Parsec.Pos
import Text.Parsec.Prim
import Text.ParserCombinators.Parsec 
import Control.Applicative ((<*), (*>), (<$>), (<*>))
import Verification.Antimirov
type Name = String  

-- EXPRESSIONS
data Expr
  = Unit 
  | Int Int
  | Str String
  | Boolean Bool 
  | Float Float
  | Var Name
  | Lambda [Pattern] Expr
  | Binops String Expr Expr
  | App Name [Expr]
  | If [(Expr, Expr)] Expr
  | Let [Def] Expr
  | Fold Expr Expr Expr
  | Lift Expr Expr -- lift
  | Lift2 Expr [Expr] -- lift2
  | Lift3 Expr [Expr] -- lift3
  | Sync Expr 
  | Prior Int Expr 
  | List [Expr]
  | EFF String Expr
  deriving (Show, Eq)

-- DEFINITIONS
data Def = Define Name [Pattern] Expr 
  deriving (Show, Eq)

-- PATTERN
data Pattern
  = PAnything
  | PStr String
  | PInt Int
  | PVar Name
  | PUnit
  deriving (Show, Eq)

-- TYPE
data Type 
  = TString 
  | TInt 
  | TBool 
  | TUnit
  | TSignal Type 
  | TFun Type Type -- function type from BasicType to BasicType

  deriving (Show, Eq)

-- DECLARATIONS
data Decl
  = Annotation Name Type
  | Definition Name [Pattern] Expr
  | Import [String] [String]  -- import name, hiding
  | EFFECT Name [Pattern] Effect Effect
  deriving (Show, Eq)

--------------------------------------------------

only_isSpace a = if a == ' ' then True else False
only_space  = satisfy only_isSpace
only_spaces = skipMany only_space 
--------------------------------------
 
---------------------------------------------------
lexeme_spa :: Parser a -> Parser a
lexeme_spa p = do
           x <- p <* only_spaces
           return x

lexeme :: Parser a -> Parser a
lexeme p = do
           x <- p <* spaces
           return x

--------------------------------------
lowVar :: Parser String
lowVar = do
    fc  <- oneOf fch
    r   <- many $ oneOf rest
    return $ ([fc] ++ r)
    where fch =  ['a'..'z'] 
          rest =  fch ++ ['0'..'9'] ++ ['A'..'Z'] ++ "_"

lowVar_Not_Key :: Parser Name
lowVar_Not_Key= do 
  x <- lookAhead lowVar
  case x of 
    "let" -> fail "a reserved keyword"
    "in" -> fail "a reserved keyword"
    "case" -> fail "a reserved keyword"
    "of" -> fail "a reserved keyword"
    "if" -> fail "a reserved keyword"
    "else" -> fail "a reserved keyword"
    "then" -> fail "a reserved keyword"
    "type" -> fail "a reserved keyword"
    "sync" -> fail "a reserved keyword"
    "prior" -> fail "a reserved keyword"
    "fold" -> fail "a reserved keyword"
    "lift" -> fail "a reserved keyword"
    "lift_2" -> fail "a reserved keyword"
    "lift_3" -> fail "a reserved keyword"
    "effect" -> fail "a reserved keyword"
    "otherwise" -> fail "a reserved keyword"
    otherwise -> string x

uppVar_Not_Key :: Parser Name
uppVar_Not_Key= do 
  x <- lookAhead uppVar
  case x of 
    "False" -> fail "a reserved keyword"
    "True" -> fail "a reserved keyword"
    otherwise -> string x
    
uppVar :: Parser String
uppVar = do
    fc  <- oneOf fch
    r   <- many $ oneOf rest
    return $ ([fc] ++ r)
    where fch =   ['A'..'Z'] 
          rest =  fch ++ ['0'..'9'] ++ ['a'..'z'] ++ "_"

name :: Parser String
name = do 
    fc  <- oneOf fch
    r   <- many $ oneOf rest
    return $ ([fc] ++ r)
    where fch =   ['A'..'Z'] ++ ['a'..'z'] ++ "_"
          rest =  fch ++ ['0'..'9'] 

numinStr :: Parser String
numinStr = do 
    fc  <- oneOf fch
    r   <- many $ oneOf rest
    return $ ([fc] ++ r)
    where fch =   ['1'..'9'] 
          rest =   ['0'..'9'] 

str :: Parser String-- Char
str = do
    fc    <- char '\"'
    mc  <- many $ oneOf anyChar
    ec   <- lexeme $ char '\"'
    return $ mc
  where anyChar = ['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9']
