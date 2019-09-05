module Generator.CAST where
import Text.ParserCombinators.Parsec 
import Control.Applicative ((<*), (*>), (<$>), (<*>))

data CAST = CAstProgram [Int]
    deriving (Eq, Show)

data CState
    = EmptyState
    | CStateBlock [CState]
    | CVariable [CExpr] CSemi
    | CFunction String [String] CState
    | CIfElse CExpr CState CState
    | CReturn (Maybe CExpr) CSemi
    | CWhile CExpr CState

data CExpr
    = EmptyExpr
    | CId String
    | CInt String
    | CBool String 
    | CString String
    | CRecord [(String, CExpr)] 
    | CList [CExpr]

data CSemi = CSemi 
    deriving ( Eq, Show)