module SignalGraph.DataType where
import Parser.AST 

preDefinedIn :: [(String, Type)]
preDefinedIn = 
    [ ("Env.motion", TBool)
    , ("Env.temprature" , TInt)
    ]

preDefinedOut :: [(String, Type)]
preDefinedOut = 
    [ ("lcd", TString)
    , ("led", TBool)
    ]

data SignalGraph = SG [SignalNode] deriving (Show, Eq)

type Methord = Expr
type NM = String
type Accumulator = Expr

data SignalNode = 
    Source NM Type [Expr]  -- input signal type, param, name 
    | LiftN NM Int Methord Type [SignalNode] -- liftn 
    | FoldP NM Methord Type Accumulator SignalNode
    | SYNC SignalNode
    | PRIOR SignalNode
    | IoN NM Type Expr SignalNode
    deriving (Show, Eq)
    --   ouput      input
