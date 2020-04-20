module Error
    (   errorHandler,
        epiFail,
        epiWin,
        ExceptionType (..)
    ) where

import Control.Exception
import System.Exit
--import Lib

data ExceptionType = ArgError String
               | RuntimeError String
               | HelpException
               deriving (Show)

instance Exception ExceptionType

help :: IO ()
help = mapM_ putStrLn [ "Usage:",
                        "\t./wolfram [-h]",
                        "\t./wolfram [--rule nbr, --start nbr, --lines nbr, --window nbr, --move nbr]",
                        "",
                        "Description:",
                        "\tWolfram is a program that implement elementary cellular automaton",
                        "\tin the terminal." ]

epiFail :: IO ()
epiFail = exitWith $ ExitFailure 84

epiWin :: IO()
epiWin = exitSuccess

errorHandler :: ExceptionType -> IO ()
errorHandler (ArgError s) = putStrLn ("Argument Error : " ++ s) >> epiFail
errorHandler (RuntimeError s) = putStrLn ("Runtime Error : " ++ s) >> epiFail
errorHandler HelpException = help >> epiWin
