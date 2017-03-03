module Semantics.Basis (
    basisAlias,
    bindBasis,
) where

import Semantics.Solver (
  SymbolTable,
  Binding(..),
  insertFunctions,
  )
import Model (
  QualifiedName,
  qualify,
  TaggedName(..),
  )
import Semantics.Alias (Alias, rename)
import Grammars.Smudge (
  StateMachine,
  Annotated(..),
  StateMachineDeclarator(..),
  )

import Data.Map (fromList)

basisAlias :: String -> Alias QualifiedName
basisAlias "" = mempty
basisAlias namespace = fromList $ map q [
            -- add more here
            "free",
            "panic_print",
            "panic"]
    where q n = (qualify n, qualify(namespace, n))

bindBasis :: Alias QualifiedName -> [StateMachine QualifiedName] -> SymbolTable
bindBasis aliases sms = mappend exports externs
    where rename' = rename aliases . qualify
          void = TagBuiltin $ qualify "void"
          str = TagBuiltin $ qualify "char"
          wrapper smName = TagState $ qualify (smName, "Event_Wrapper") -- a horrible kludge
          exports = insertFunctions mempty Exported $ concat [[
            -- add more sm-specific exports here
            (qualify (smName, "Free_Message"),       ([wrapper smName], "")),
            (qualify (smName, "Handle_Message"),     ([wrapper smName], "")),
            (qualify (smName, "Current_state_name"), ([], "char"))]
                | Annotated _ (StateMachineDeclarator smName) <- sms]
          externs = insertFunctions mempty External $ concat [[
            -- add more sm-specific externals here
            (qualify (smName, "Send_Message"), ([wrapper smName], ""))]
                | Annotated _ (StateMachineDeclarator smName) <- sms] ++ [
            -- add more smudge-wide externals here
            (rename' "free",        ([void], "")),
            (rename' "panic_print", ([str, str, str], "")),
            (rename' "panic",       ([], ""))]
