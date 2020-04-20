module Lib
    (
        Token (..),
        WolfArgs (..),
        updateWindow,
        updateMove,
        updateRule,
        updateLines,
        updateStart,
        Rule,
        Line,
        Cell,
        notNeg,
        readValue
    ) where

import Control.Exception
import Error

type Rule = Cell -> Cell -> Cell -> Cell
type Start = Int
type Lines = Int
type Window = Int
type Move = Int

data WolfArgs = WolfArgs (Maybe Rule) Start Lines Window Move

data Token = Value Int | RULE | START | LINES | WINDOW | MOVE deriving (Show)

type Cell = Char
type Before = [Cell]
type DisplayWindow = [Cell]
type After = [Cell]
type Line = (Before, DisplayWindow, After)

-- Update WolfArgs type
updateRule :: WolfArgs -> Rule -> WolfArgs
updateRule   (WolfArgs _ start lines window move) rule = WolfArgs (Just rule) start lines window move

updateStart :: WolfArgs -> Start -> WolfArgs
updateStart  (WolfArgs rule _ lines window move) start = WolfArgs rule start lines window move

updateLines :: WolfArgs -> Lines -> WolfArgs
updateLines  (WolfArgs rule start _ window move) lines = WolfArgs rule start lines window move

updateWindow :: WolfArgs -> Window -> WolfArgs
updateWindow (WolfArgs rule start lines _ move) window = WolfArgs rule start lines window move

updateMove :: WolfArgs -> Move -> WolfArgs
updateMove   (WolfArgs rule start lines window _)      = WolfArgs rule start lines window
--

notNeg :: Token -> Token
notNeg (Value a)
  | a < 0 = throw $ ArgError "Value cannot be negative."
  | otherwise = Value a

readValue :: Maybe Int -> Int
readValue (Just a) = a
readValue Nothing  = throw $ ArgError "Bad argument, must be Int."
