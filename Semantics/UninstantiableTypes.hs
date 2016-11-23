{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

module Semantics.UninstantiableTypes (
    UninstantiableTypes
) where

import Grammars.Smudge (Annotated(..))
import Model (TaggedName, mangleWith, qualify)
import Semantics.Solver (Ty, instantiable, SymbolTable)
import Semantics.Semantic (Passable(..), Severity(..), Fault(..))

data UninstantiableTypes = UninstantiableTypes [(TaggedName, Ty)]
instance Monoid UninstantiableTypes where
    mempty = UninstantiableTypes mempty
    mappend (UninstantiableTypes tys) (UninstantiableTypes tys') =
        UninstantiableTypes (mappend tys tys')

instance Passable UninstantiableTypes where
    type Representation UninstantiableTypes = SymbolTable
    accumulate (_, (_, tau)) a | instantiable tau = a
    accumulate (n, (_, tau)) a                    = mappend (UninstantiableTypes [(n, tau)]) a
    test (Annotated pos _, _) (UninstantiableTypes tys) =
        case tys of
        [] -> []
        ts -> [Fault ERROR pos $ "uninstantiable type: " ++
               mangleWith ((++) . (++ ".")) id (qualify n) ++ " : " ++ show t | (n, t) <- ts]
