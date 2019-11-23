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

type SignalGraph = [OutputNode] 

type Method = Expr
type NM = String
type Accumulator = Expr
type Port = Int

type OutputNode =  (NM, Type, Port, SignalTerm) -- int -> Signal -> IO()

data SignalTerm = 
      ConstStr String
    | ConstInt Int
    | Source NM Type Port -- [Expr]  -- input signal type, param, name 
    | LetSignal NM SignalTerm
    | LiftN  NM Type Int Method [SignalTerm] -- liftn 
    | FoldP  NM Type Method Accumulator SignalTerm -- 
    | SYNC   SignalTerm
    | PRIOR  Int SignalTerm  
    | Nonode
    deriving (Show, Eq)

