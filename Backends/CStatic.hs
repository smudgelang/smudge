module Backends.CStatic where

import Backends.Backend (Backend(..))
import Grammars.Smudge (StateMachine(..), State(..), Event(..), SideEffect(..), Happening(..))
import Grammars.C89
import Unparsers.C89 (renderPretty)

import Data.Graph.Inductive.Graph (labNodes)
import System.Console.GetOpt
import System.FilePath (FilePath, dropExtension, (<.>))

data CStaticOption = OutFile FilePath
    deriving (Show, Eq)

stateEnum :: StateMachine -> [State] -> Declaration
stateEnum (StateMachine smName) ss =
    Declaration
    (fromList [A TYPEDEF,
               B (makeEnum smMangledName ssMangled)])
    (Just $ fromList [InitDeclarator (Declarator Nothing (IDirectDeclarator smMangledName)) Nothing])
    SEMICOLON
    where
        smMangledName = mangleIdentifier smName ++ "_State"
        ssMangled = [mangleIdentifier (smName ++ show s) | s <- ss]

makeEnum :: Identifier -> [Identifier] -> TypeSpecifier
makeEnum smName [] = ENUM (Left $ smName)
makeEnum smName ss = 
    ENUM (Right (Quad (Just $ smName)
    LEFTCURLY
    (fromList [Enumerator s Nothing | s <- ss])
    RIGHTCURLY))

instance Backend CStaticOption where
    options = ("c",
               [Option [] ["o"] (ReqArg OutFile "FILE")
                 "The name of the target file if not derived from source file."])
    generate os gs inputName = writeTranslationUnit tu (outputName os)
        where tu = fromList $ graphToCode gs
              graphToCode gs = [(ExternalDeclaration (Right $ stateEnum (StateMachine "state machine name") (states g))) | g <- gs]
              states g = [name | (_, name) <- labNodes g]
              writeTranslationUnit u fp = (writeFile fp (renderPretty u)) >> (return fp)
              getFirstOrDefault :: ([a] -> b) -> b -> [a] -> b
              getFirstOrDefault _ d     [] = d
              getFirstOrDefault f _ (x:xs) = f xs
              outputName ((OutFile f):_) = f
              outputName xs = getFirstOrDefault outputName ((dropExtension inputName) <.> "c") xs
