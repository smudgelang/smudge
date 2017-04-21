{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

module Semantics.UninstantiableTypes (
    UninstantiableTypes
) where

import Grammars.Smudge (StateMachine(..))
import Model (TaggedName, qualify)
import Parsers.Id (at)
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
    test (StateMachine sm_name, _) (UninstantiableTypes tys) =
        case tys of
        [] -> []
        ts -> [Fault ERROR (at sm_name) $ "uninstantiable type: " ++
               show (qualify n) ++ " : " ++ show t | (n, t) <- ts]
