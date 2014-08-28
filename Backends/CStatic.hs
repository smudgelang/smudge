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

--Blah :: Gr State Happening -> TypeSpecifier

stateEnum :: StateMachine -> [State] -> TypeSpecifier
stateEnum (StateMachine smName) ss =
    case ss of
        [] ->
             ENUM (Left smMangledName)
        otherwise ->
             ENUM (Right (Quad (Just smMangledName)
             LEFTCURLY
             (fromList $ map ((flip Enumerator Nothing) . mangleIdentifier . ((smName ++) . show)) (ss))
             RIGHTCURLY))
    where smMangledName = mangleIdentifier (smName ++ "State")

instance Backend CStaticOption where
    options = ("c",
               [Option [] ["o"] (ReqArg OutFile "FILE")
                 "The name of the target file if not derived from source file."])
    generate os g inputName = writeTranslationUnit tu (outputName os)
        where tu = fromList [ExternalDeclaration (Right $ Declaration (fromList [B $ stateEnum (StateMachine "state machine name") states]) Nothing SEMICOLON)]
              states = map (\ (_, name) -> name) $ labNodes (g !! 0)
              writeTranslationUnit u fp = (writeFile fp (renderPretty u)) >> (return fp)
              getFirstOrDefault :: ([a] -> b) -> b -> [a] -> b
              getFirstOrDefault _ d     [] = d
              getFirstOrDefault f _ (x:xs) = f xs
              outputName ((OutFile f):_) = f
              outputName xs = getFirstOrDefault outputName ((dropExtension inputName) <.> "c") xs
