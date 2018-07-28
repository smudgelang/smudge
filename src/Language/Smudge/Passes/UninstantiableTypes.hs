-- Copyright 2017 Bose Corporation.
-- This software is released under the 3-Clause BSD License.
-- The license can be viewed at https://github.com/Bose/Smudge/blob/master/LICENSE

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

module Language.Smudge.Passes.UninstantiableTypes (
    UninstantiableTypes
) where

import Language.Smudge.Semantics.Model (TaggedName, qualify)
import Language.Smudge.Parsers.Id (at)
import Language.Smudge.Semantics.Solver (Ty, instantiable, SymbolTable)
import Language.Smudge.Passes.Passes (Passable(..), Severity(..), Fault(..))

data UninstantiableTypes = UninstantiableTypes [(TaggedName, Ty)]
instance Semigroup UninstantiableTypes where
    (UninstantiableTypes tys) <> (UninstantiableTypes tys') =
        UninstantiableTypes (tys <> tys')
instance Monoid UninstantiableTypes where
    mempty = UninstantiableTypes mempty
    mappend = (<>)

instance Passable UninstantiableTypes where
    type Representation UninstantiableTypes = SymbolTable
    accumulate (_, (_, tau)) a | instantiable tau = a
    accumulate (n, (_, tau)) a                    = UninstantiableTypes [(n, tau)] <> a
    test _ (UninstantiableTypes tys) =
        case tys of
        [] -> []
        ts -> [Fault ERROR (at n) $ "uninstantiable type: " ++
               show (qualify n) ++ " : " ++ show t | (n, t) <- ts]
