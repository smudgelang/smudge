module Semantics.Basis (
    bindBasis,
) where

import Semantics.Solver (SymbolTable, insertFunctions)
import Model (QualifiedName, qualify)
import Semantics.Alias (Alias, rename)

bindBasis :: Alias QualifiedName -> SymbolTable
bindBasis aliases =
    let rename' = rename aliases . qualify
    in  insertFunctions mempty  [
            (rename' "panic_print", (["char", "char", "char"], "")),
            (rename' "panic",       ([], ""))
            ]
