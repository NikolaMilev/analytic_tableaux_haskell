module Main where
import Formula
import NNF
import AnalyticTableaux
import Parser
import System.Console.Haskeline
main :: IO ()
main = do runInputT defaultSettings loop
   where
       loop :: InputT IO ()
       loop = do
           minput <- getInputLine ">>> "
           case minput of
               Nothing -> return ()
               Just "quit" -> return ()
               Just "help" -> do 
               					outputStrLn "This program can check if a formula is a tautology or satisfiable."
               					outputStrLn "In order to check if a formula is a tautology, type taut and then type your formula."
               					outputStrLn "Permitted formula operators are ~, /\\, \\/, =>, <=> and parentheses."
               					outputStrLn "Example of correct use: "
               					outputStrLn "   >>> taut p => p"
               					outputStrLn "   taut (p => p) : True"
             					loop
               Just input -> do outputStrLn $ parseString input
                                loop  		
--outputStrLn "Welcome to Analytic Tableaux!" 
--outputStrLn "Type help for help or quit to quit."
-- outputStrLn "This program can check if a formula is a tautology or satisfiable."
-- outputStrLn "In order to check if a formula is a tautology, type taut and then type your formula."