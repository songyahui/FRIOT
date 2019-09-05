import Parser.Parser as P
import SignalGraph.SignalGraph 
import Generator.CGenerator
import Text.ParserCombinators.Parsec 
import System.IO 
import System.Environment 

main :: IO ()
main = do
    args <- getArgs
    inFile <- openFile (head args) ReadMode 
    outFile <- openFile (args !! 1) ReadWriteMode 
    inpStr <- hGetContents inFile

    case P.parse (head args) inpStr of 
        Left ep -> do print ep 
        Right astp -> do 
            print astp 
            putStrLn "-----------Parse succecfully!-----------" 
            print $ signalGraph astp
            putStrLn "-----------Transform succecfully!-----------" 
            print $ c_generator astp (signalGraph astp) 
               -- print $ generator $ transformer astp []
            writeFile (args !! 1) ( c_generator astp (signalGraph astp) )
            hClose inFile
            --hClose outFile
