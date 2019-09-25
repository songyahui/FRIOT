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
      NoNode 
    | Source NM Type [Expr]  -- input signal type, param, name 
    | LiftN  NM Type Methord [SignalNode] -- liftn 
    | FoldP  NM Type Methord Accumulator SignalNode -- 
    | IoN    NM Type Expr SignalNode -- int -> Signal -> IO()
    | SYNC   SignalNode
    | PRIOR  SignalNode  
    deriving (Show, Eq)
    --   ouput      input
