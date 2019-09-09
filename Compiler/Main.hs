import Parser.Parser as P
--import SignalGraph.SignalGraph 
--import Generator.CGenerator
import Text.ParserCombinators.Parsec 
import System.IO 
import System.Environment 
import Verification.Verification

pathIn = "../testcases/Friot/"
pathOut = "../testcases/C/"

main :: IO ()
main = do
    args <- getArgs
    inFile <- openFile (pathIn ++ (head args) ++ ".hs") ReadMode 
    outFile <- openFile (pathOut ++ (head args) ++ ".c") ReadWriteMode 
    inpStr <- hGetContents inFile

    case P.parse (head args) inpStr of 
        Left ep -> do print ep 
        Right astp -> do 
            print astp 
            putStrLn "-----------Parse succecfully!-----------" 
            verification astp
            putStrLn "-----------Verified succecfully!-----------" 
            --print $ signalGraph astp
            --putStrLn "-----------Transform succecfully!-----------" 
            --print $ c_generator astp (signalGraph astp) 
               -- print $ generator $ transformer astp []
            --writeFile (pathOut ++ (head args) ++ ".c") ( c_generator astp (signalGraph astp) )
            hClose inFile
            --hClose outFile
