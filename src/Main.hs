{-|
  Module      : Main
  Description : The Main module of the program.
  Copyright   : (c) Nikola Milev, 2017.
  License     : None
  Maintainer  : nikola.n.milev@gmail.com
  Stability   : Stable
  Portability : Any platform that supports cabal, except for the .cabal file itself.

  In this module, we do the basic I/O operations and give the read data to the parser that does the analysis of the formulae.
-}

module Main where
import Formula
import NNF
import AnalyticTableaux
import Parser
import System.Console.Haskeline
{-|
  The main function that parses the input, using functions from System.Console.Haskeline module, 
  forwarding it to he Formula parser.
-}
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