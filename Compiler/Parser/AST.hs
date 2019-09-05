module Parser.AST where
import Data.Char
import Text.Parsec.Pos
import Text.Parsec.Prim
import Text.ParserCombinators.Parsec 
import Control.Applicative ((<*), (*>), (<$>), (<*>))

type Name = String  

-- EXPRESSIONS
data Expr
  = Str String
  | Int Int
  | Float Double 
  | Boolean String 
  | Var String
  | List [Expr]
  | Negate Expr 
  | Binops String Expr Expr
  | Lambda [Pattern] Expr
  | Call String [Expr]
  | If [(Expr, Expr)] Expr
  | Let [Def] Expr
  | Case Expr [(Pattern, Expr)]
  | Tupple Expr Expr [Expr] 
  | Record [(String, Expr)]
  | Update String [(String, Expr)]
  | Tag String [Expr]
  | Block [Expr]
  | Parent Expr
  deriving (Show, Eq)

-- DEFINITIONS
data Def = Define (String) [Pattern] Expr 
  deriving (Show, Eq)

-- PATTERN
data Pattern
  = PAnything
  | PStr String
  | PInt Int
  | PVar String
  | PCtor String [Pattern]
  | PCons Pattern Pattern
  | PList [Pattern]   
  | PRecord [String]  --4 ?? 
  | PTuple Pattern Pattern [Pattern] 
  | PUnit
  deriving (Show, Eq)

-- TYPE
data Type
  = TVar Name -- lowercase, type variable
  | TString | TInt | TBool | TIO | TUnit
  | TLambda [Type]  -- t1 -> t2
  | TTuple Type Type [Type] -- at lease 2 
  | TList Type
  | TApp [Type]
  deriving (Show, Eq)

-- DECLARATIONS
data Decl
  = Union Name [String] [(Name, [Type])]
  | Alias Name [String] Type
  | Annotation Name Type
  | Definition Name [Pattern] Expr
  | Import [String] [String]  -- import name, hiding
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
