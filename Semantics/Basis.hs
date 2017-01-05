module Semantics.Basis (
    basisAlias,
    bindBasis,
) where

import Semantics.Solver (SymbolTable, insertFunctions)
import Model (QualifiedName, qualify)
import Semantics.Alias (Alias, rename)

import Data.Map (fromList)

basisAlias :: String -> Alias QualifiedName
basisAlias "" = mempty
basisAlias namespace = fromList $ map q [
            -- add more here
            "panic_print",
            "panic"]
    where q n = (qualify n, qualify(namespace, n))

bindBasis :: Alias QualifiedName -> SymbolTable
bindBasis aliases =
    let rename' = rename aliases . qualify
    in  insertFunctions mempty  [
            -- add more here
            (rename' "panic_print", (["char", "char", "char"], "")),
            (rename' "panic",       ([], ""))]
