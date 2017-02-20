module Semantics.Basis (
    basisAlias,
    bindBasis,
) where

import Semantics.Solver (
  SymbolTable,
  Binding(..),
  insertFunctions,
  )
import Model (QualifiedName, qualify)
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
          exports = insertFunctions mempty Exported [
            -- add more here
            (qualify (smName, "Current_state_name"), ([], "char"))
                | Annotated _ (StateMachineDeclarator smName) <- sms]
          externs = insertFunctions mempty External [
            -- add more here
            (rename' "free",        (["void"], "")),
            (rename' "panic_print", (["char", "char", "char"], "")),
            (rename' "panic",       ([], ""))]
