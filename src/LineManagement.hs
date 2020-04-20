module LineManagement
    (   firstLine,
        beforeLine,
     ) where

import Lib

firstLine :: Int -> Line
firstLine w = ([], replicate (div w 2) ' ' ++ '*' : replicate (div w 2) ' ', [])

cellListTruple :: [Cell] -> ([Cell],  [Cell]) -> Line
cellListTruple a (b, c) = (a, b, c)

beforeLine :: Rule -> Line -> Line
beforeLine rule ([], l@(' ' : _), la)   = cellListTruple []                       $ windowLine ' ' rule l la
beforeLine rule ([], l@('*' : _), la)   = cellListTruple [' ']                    $ windowLine ' ' rule l la
beforeLine rule ([e1], l, la)           = cellListTruple [' ', e1]                $ windowLine e1  rule l la
beforeLine rule (lc, l@(e3 : _), la)    = cellListTruple (beforeLine' rule lc e3) $ windowLine (last lc) rule l la

beforeLine' :: Rule -> [Cell] -> Cell -> [Cell]
beforeLine' rule [e1, e2] we                = ' ' : rule ' ' e1 e2 : rule e1 e2 we : []
beforeLine' rule [e1, e2, e3] we            = ' ' : rule ' ' e1 e2 : rule e1 e2 e3 : rule e2 e3 we : []
beforeLine' rule (e1 : l@(e2 : e3 : _)) we  = ' ' : rule ' ' e1 e2 : rule e1 e2 e3 : beforeLine'' rule l we

beforeLine'' :: Rule -> [Cell] -> Cell -> [Cell]
beforeLine'' rule [e1, e2] we                = [rule e1 e2 we]
beforeLine'' rule (e1 : l@(e2 : e3 : _)) we  = rule e1 e2 e3 : beforeLine'' rule l we


windowLine :: Cell -> Rule -> [Cell] -> [Cell] -> ([Cell], [Cell])
windowLine e1 rule l@(e2 : e3 : _) [] = (rule e1 e2 e3 : windowLine' rule l  ' ', afterLine rule (last l) [] )
windowLine e1 rule l@(e2 : e3 : _) la = (rule e1 e2 e3 : windowLine' rule l (head la), afterLine rule (last l) la)

windowLine' :: Rule -> [Cell] -> Cell -> [Cell]
windowLine' rule [e1, e2] ae              = [rule e1 e2 ae]
windowLine' rule (e1 : l@(e2: e3: _)) ae  = rule e1 e2 e3 : windowLine' rule l ae

afterLine :: Rule -> Cell -> [Cell] -> [Cell]
afterLine rule ' ' []              = []
afterLine rule '*' []              = [' ']
afterLine rule e1  [e2]            = [rule e1 e2 ' ', ' ']
afterLine rule e1  [e2, e3]        = [rule e1 e2 e3, rule e2 e3 ' ', ' ']
afterLine rule e1  l@(e2 : e3 :_)  = rule e1 e2 e3 : afterLine' rule l

afterLine' :: Rule -> [Cell] -> [Cell]
afterLine' rule [e1, e2]                = rule e1 e2 ' ' : [' ']
afterLine' rule (e1 : l@(e2 : e3 : _))  = rule e1 e2 e3 : afterLine' rule l

