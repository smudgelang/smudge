module Main where

import Text.ParserCombinators.Parsec
import System.Environment
import Grammar

result :: Either ParseError (String, [(String, [(String, Maybe String)])]) -> IO ()
result (Left err) = print err
result (Right s)  = print s

main = do
    args <- getArgs
    let fileName = head args
    compilationUnit <- readFile fileName
    result $ parse state_machine fileName compilationUnit
