module Generator.JSAST where
import Text.ParserCombinators.Parsec 
import Control.Applicative ((<*), (*>), (<$>), (<*>))

data JSAST = JSAstProgram [JSState]
    deriving (Eq, Show)

data JSState
    = Empty -- help to dubug
    | JSStateBlock [JSState]      -- ^{stmts};
    | JSVariable [JSExpr] JSSemi  -- var exprs;
    | JSFunction String [String] JSState  -- ^fn,name,(parameters)block
    | JSIfElse JSExpr JSState JSState -- ^if,(,expr,),stmt,else,rest
    | JSReturn (Maybe JSExpr) JSSemi
    | JSWhile JSExpr JSState -- ^while,lb,expr,rb,stmt
    | JSCallDot JSExpr JSExpr JSSemi
    | JSStateList [JSExpr]
      deriving ( Eq, Show)

data JSExpr
    = Empty1
    | JSId String
    | JSInt String
    | JSBool String 
    | JSString String
    | JSIndex JSExpr JSExpr --model[0]
    -----------------------------------
    | JSTagList [JSExpr]
    | JSList [JSExpr]
    | JSVarInitExpr JSExpr JSExpr -- id = initializer
    | JSRecord [(String, JSExpr)] 
    | JSMemberDot JSExpr JSExpr -- firstpart.name
    | JSMemberExpr JSExpr [JSExpr] -- expr(args)
    | JSMemberNew JSExpr [JSExpr] -- new, name(args)
    | JSExprBinary JSExpr JSBinOp JSExpr -- lhs, op, rhs
    | JSFunctionExpression String [String] [JSExpr] -- ^fn,name,lb, parameter list,rb,block`
      deriving ( Eq, Show)

data JSBinOp
    = Divide  -- /
    | Eq      -- =
    | Ge      -- >=
    | Gt      -- >
    | Le      -- <=
    | Lt      -- <
    | Minus   -- -
    | Neq     -- =
    | Plus    -- +
    | Times   -- *
    | Andand  -- && 
    | EqEq    -- ==
    deriving ( Eq, Show)

data JSSemi = Semi 
    deriving ( Eq, Show)
