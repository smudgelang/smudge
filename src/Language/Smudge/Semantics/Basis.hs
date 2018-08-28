-- Copyright 2018 Bose Corporation.
-- This software is released under the 3-Clause BSD License.
-- The license can be viewed at https://github.com/smudgelang/smudge/blob/master/LICENSE

module Language.Smudge.Semantics.Basis (
    basisAlias,
    bindBasis,
) where

import Language.Smudge.Semantics.Solver (
  SymbolTable,
  Binding(..),
  insertFunctions,
  )
import Language.Smudge.Semantics.Model (
  QualifiedName,
  qualify,
  TaggedName,
  Tagged(..),
  )
import Language.Smudge.Semantics.Alias (Alias, rename)
import Language.Smudge.Grammar (
  StateMachine(..),
  )

import Data.Map (fromList)
import Data.Semigroup (Semigroup(..))

basisAlias :: String -> Alias QualifiedName
basisAlias "" = mempty
basisAlias namespace = fromList $ map q [
            -- add more here
            "debug_print",
            "free",
            "panic_print",
            "panic"]
    where q n = (qualify n, qualify(namespace, n))

bindBasis :: Alias QualifiedName -> [StateMachine QualifiedName] -> SymbolTable
bindBasis aliases sms = exports <> externs
    where rename' = rename aliases . qualify
          void = TagBuiltin $ qualify "void"
          str = TagBuiltin $ qualify "char"
          wrapper smName = TagState $ qualify (smName, "Event_Wrapper") -- a horrible kludge
          exports = insertFunctions mempty Exported $ concat [[
            -- add more sm-specific exports here
            (qualify (smName, "Free_Message"),       ([wrapper smName], "")),
            (qualify (smName, "Handle_Message"),     ([wrapper smName], "")),
            (qualify (smName, "Current_state_name"), ([], "char"))]
                | StateMachine smName <- sms]
          externs = insertFunctions mempty External $ concat [[
            -- add more sm-specific externals here
            (qualify (smName, "Send_Message"), ([wrapper smName], ""))]
                | StateMachine smName <- sms] ++ [
            -- add more smudge-wide externals here
            (rename' "debug_print", ([str, str, str], "")),
            (rename' "free",        ([void], "")),
            (rename' "panic_print", ([str, str, str], "")),
            (rename' "panic",       ([], ""))]
