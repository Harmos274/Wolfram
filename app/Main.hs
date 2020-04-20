module Main where

-- System header
import Control.Exception
import System.Environment
import Text.Read
--

-- Project header
import Error (errorHandler)
import Rules (callRule)
import Lib (Line, Cell, Token, WolfArgs (..))
import ArgsManager (wolframLexer, foldArg)
--

initArgs = WolfArgs Nothing 0 0 80 0


showLine :: Line -> [Cell]
showLine (_, l, _) = l

main :: IO ()
main = handle errorHandler $ getArgs >>= (mapM_  (putStrLn . showLine) . wolfram)

wolfram :: [String] -> [Line]
wolfram args = callRule $ parse $ wolframLexer args

parse :: [Token] -> (Maybe Token, WolfArgs)
parse = foldl foldArg (Nothing, initArgs)

