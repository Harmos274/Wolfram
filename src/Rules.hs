module Rules
    (
        rule30,
        rule90,
        rule110,
        callRule
    ) where

-- System
import Control.Exception
--

-- Project
import LineManagement
import Error
import Lib
--

callRule :: (Maybe Token, WolfArgs) -> [Line]
callRule (_, (WolfArgs (Just r) start 0     window move))  = calculate 1000000 r $ firstLine window
callRule (_, (WolfArgs (Just r) start lines window move))  = drop start $ calculate lines r $ firstLine window
callRule (_, (WolfArgs Nothing  start lines window move))  = throw $ ArgError "No rule provided."

calculate :: Int -> Rule -> Line -> [Line]
calculate 0 _ l  = [l]
calculate i f l  = l : calculate (i - 1) f (beforeLine f l)

rule30 :: Rule
rule30 '*' '*' '*' = ' '
rule30 '*' '*' ' ' = ' '
rule30 '*' ' ' ' ' = '*'
rule30 ' ' '*' '*' = '*'
rule30 ' ' '*' ' ' = '*'
rule30 ' ' ' ' '*' = '*'
rule30 ' ' ' ' ' ' = ' '
rule30 '*' ' ' '*' = ' '
rule30  _   _   _  = throw $ RuntimeError "Impossible has happened."

rule90 :: Rule
rule90 '*' '*' '*' = ' '
rule90 '*' '*' ' ' = '*'
rule90 '*' ' ' ' ' = '*'
rule90 ' ' '*' '*' = '*'
rule90 ' ' '*' ' ' = ' '
rule90 ' ' ' ' '*' = '*'
rule90 ' ' ' ' ' ' = ' '
rule90 '*' ' ' '*' = ' '
rule90  _   _   _  = throw $ RuntimeError "Impossible has happened."

rule110 :: Rule
rule110 '*' '*' '*' = ' '
rule110 '*' '*' ' ' = '*'
rule110 '*' ' ' ' ' = ' '
rule110 ' ' '*' '*' = '*'
rule110 ' ' '*' ' ' = '*'
rule110 ' ' ' ' '*' = '*'
rule110 ' ' ' ' ' ' = ' '
rule110 '*' ' ' '*' = '*'
rule110  _   _   _  = throw $ RuntimeError "Impossible has happened."
