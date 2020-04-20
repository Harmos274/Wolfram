module ArgsManager
    (
        wolframLexer,
        foldArg,
        createTuple,
        createRuleTuple
    ) where

import Text.Read
import Control.Exception
import Rules
import Lib
import Error

wolframLexer :: [String] -> [Token]
wolframLexer [] = []
wolframLexer ("--help":_)      = throw HelpException
wolframLexer ("--h":_)         = throw HelpException
wolframLexer ("--rule": xs)    = RULE   : wolframLexer xs
wolframLexer ("--start": xs)   = START  : wolframLexer xs
wolframLexer ("--lines": xs)   = LINES  : wolframLexer xs
wolframLexer ("--window": xs)  = WINDOW : wolframLexer xs
-- wolframLexer ("--move": xs)    = MOVE   : wolframLexer xs
wolframLexer (l : xs)          = Value (readValue $ readMaybe l) : wolframLexer xs

foldArg :: (Maybe Token, WolfArgs) -> Token -> (Maybe Token, WolfArgs)
foldArg (Nothing, wa)  RULE    = (Just RULE, wa)
foldArg (Nothing, wa)  START   = (Just START, wa)
foldArg (Nothing, wa)  LINES   = (Just LINES, wa)
foldArg (Nothing, wa)  WINDOW  = (Just WINDOW, wa)
-- foldArg (Nothing, wa)  MOVE    = (Just MOVE, wa)
foldArg ((Just t), wa) a       = createTuple t (notNeg a) wa
foldArg (Nothing, wa)  _       = throw $ ArgError "Invalid directive."

createTuple :: Token -> Token -> WolfArgs -> (Maybe Token, WolfArgs)
createTuple RULE   (Value a) wa  = createRuleTuple a wa
createTuple START  (Value a) wa  = (Nothing, updateStart   wa a)
createTuple LINES  (Value a) wa  = (Nothing, updateLines   wa a)
createTuple WINDOW (Value a) wa  = (Nothing, updateWindow  wa a)
-- createTuple MOVE   (Value a) wa  = (Nothing, updateMove    wa a)
createTuple START  _ _           = throw $ ArgError "Bad start arguments."
createTuple LINES  _ _           = throw $ ArgError "Bad line arguments."
createTuple WINDOW _ _           = throw $ ArgError "Bad window arguments."
createTuple RULE   _ _           = throw $ ArgError "Bad rule arguments."
-- createTuple MOVE   _ _           = throw $ ArgError "Bad move arguments."

createRuleTuple :: Int -> WolfArgs -> (Maybe Token, WolfArgs)
createRuleTuple 30 wa   = (Nothing, updateRule wa rule30)
createRuleTuple 90 wa   = (Nothing, updateRule wa rule90)
createRuleTuple 110 wa  = (Nothing, updateRule wa rule110)
createRuleTuple _ wa    = throw $ ArgError "Rule must be 30, 90 or 110."
