module Main where

import Text.ParserCombinators.Parsec (parse, ParseError)
import System.Environment
import Grammar

result :: Either ParseError (StateMachine, [(State, [(Event, ([(Maybe String, Maybe (StateMachine, Event))], State))])]) -> IO()
result (Left err) = print err
result (Right s)  = print s

main = do
    args <- getArgs
    let fileName = head args
    compilationUnit <- readFile fileName
    result $ parse state_machine fileName compilationUnit
