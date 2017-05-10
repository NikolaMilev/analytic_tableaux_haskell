module Main where
import Formula
import NNF
import AnalyticTableaux
import Parser
import System.Console.Haskeline
main :: IO ()
main = runInputT defaultSettings loop
   where
       loop :: InputT IO ()
       loop = do
           minput <- getInputLine ">>> "
           case minput of
               Nothing -> return ()
               Just "quit" -> return ()
               Just input -> do outputStrLn $ parseString input
                                loop  		