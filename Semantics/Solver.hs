{-# LANGUAGE NamedFieldPuns #-}

module Semantics.Solver (
    Ty(..),
    Binding(..),

    SymbolTable,
    insertExternalSymbol,
    toList,
    (!),

    passGraphWithSymbols,
    passUniqueSymbols,
) where

import Grammars.Smudge (
  Name,
  Annotated(..),
  StateMachine(..),
  Event(..),
  Function(..),
  )
import Model (
  Identifier(CookedId),
  QualifiedName(..),
  Tag(..),
  TaggedName,
  EnterExitState(..),
  Happening(..),
  )

import Data.Graph.Inductive.Graph (labNodes, labEdges)
import Data.Graph.Inductive.PatriciaTree (Gr)
import Data.Map (Map, fromListWith, unionsWith, insertWith)
import qualified Data.Map as Map(map, toList, (!))
import Data.Set (Set, union, singleton)
import qualified Data.Set (map)
import Control.Monad (liftM)
import Data.Maybe (fromJust)

-- external interface
data Binding = External | Unresolved | Resolved
    deriving (Show, Eq, Ord)

data Ty = Void
        | Ty Binding TaggedName
        | Unary Binding Ty Ty
    deriving (Show, Eq, Ord)

type UnfilteredSymbolTable = Map TaggedName (Set Ty)

newtype SymbolTable = SymbolTable SymTab
    deriving (Show, Eq, Ord)

instance Monoid SymbolTable where
    mappend (SymbolTable gamma) (SymbolTable gamma') = SymbolTable $ mappend gamma gamma'
    mempty = SymbolTable mempty

-- Old table, Name, args, return type, new table
insertExternalSymbol :: SymbolTable -> Name -> [Name] -> Name -> SymbolTable
insertExternalSymbol (SymbolTable gamma) fname args returnType = SymbolTable $ Data.Map.insertWith simplifyDefinitely name ty gamma
    where
          name = (TagFunction, QualifiedName [CookedId fname])
          ty = (Unary External
                      (if null args then Void else Ty External (TagBuiltin, QualifiedName (map CookedId args)))
                      (if null returnType then Void else Ty External (TagBuiltin, QualifiedName [CookedId returnType])))

toList :: SymbolTable -> [(TaggedName, Ty)]
toList (SymbolTable gamma) = Map.toList gamma

(!) :: SymbolTable -> TaggedName -> Ty
(SymbolTable gamma) ! name = gamma Map.! name

-- internal implementation
type SymTab = Map TaggedName Ty

symbols :: (StateMachine TaggedName, Gr EnterExitState Happening) -> UnfilteredSymbolTable
symbols (Annotated _ sm, gr) =
    fromListWith union $ [(n, singleton $ Unary (bnd f) (param (event h) se) (result se))
                          | (_, _, h) <- labEdges gr, se@(n, f) <- sideEffects h]
                         ++ [(n, singleton $ Unary (bnd f) (tparam se) (result se))
                             | (_, EnterExitState {en, ex}) <- labNodes gr, se@(n, f) <- en ++ ex]
                         ++ [(e, singleton $ Unary Resolved (Ty Resolved e) Void)
                             | (_, _, Happening {event = (Event e)}) <- labEdges gr]
    where
        bndQE (sm', _) | sm == sm' = Resolved
        bndQE _                    = Unresolved
        bnd (FuncEvent qe) = bndQE qe
        bnd _              = External
        param _           (_, FuncEvent qe@(_, (Event e))) = Ty (bndQE qe) e
        param _           (_, FuncEvent (_, _)) = undefined
        param (Event e)   _                     = Ty Resolved e
        param _           _                     = Void
        tparam (_, FuncEvent qe@(_, (Event e))) = Ty (bndQE qe) e
        tparam (_, FuncEvent (_, _)) = undefined
        tparam _                     = Void
        result (_, FuncTyped qe@(_, (Event e))) = Ty (bndQE qe) e
        result (_, FuncTyped (_, _)) = undefined
        result _                     = Void

passGraphWithSymbols :: [(StateMachine TaggedName, Gr EnterExitState Happening)] ->
                        ([(StateMachine TaggedName, Gr EnterExitState Happening)], UnfilteredSymbolTable)
passGraphWithSymbols sms = (sms, unionsWith union $ map symbols sms)

-- a result is simpler than no result
simplifyReturn :: Ty -> Ty -> Maybe Ty
simplifyReturn a    b | a == b = Just a
simplifyReturn a    Void       = Just a
simplifyReturn Void b          = Just b
simplifyReturn _    _          = Nothing

-- no parameters is simpler than parameters
simplifyParam :: Ty -> Ty -> Ty
simplifyParam a b | a == b = a
simplifyParam _ _          = Void

-- an external binding cannot be resolved
resolveBinding :: Binding -> Binding -> Binding
resolveBinding a          b        | a == b = a
resolveBinding Resolved   Unresolved        = Resolved
resolveBinding Unresolved Resolved          = Resolved
resolveBinding _          _                 = undefined

simplifyFunc :: Maybe Ty -> Maybe Ty -> Maybe Ty
simplifyFunc (Just (Unary ab ap ar)) (Just (Unary bb bp br)) = liftM (Unary (resolveBinding ab bb) (simplifyParam ap bp)) (simplifyReturn ar br)
simplifyFunc _                        _                      = Nothing

simplifyDefinitely :: Ty -> Ty -> Ty
simplifyDefinitely a b = fromJust (simplifyFunc (Just a) (Just b))

mapJust :: Ord a => Set a -> Set (Maybe a)
mapJust = Data.Set.map Just

passUniqueSymbols :: ([(StateMachine TaggedName, Gr EnterExitState Happening)], UnfilteredSymbolTable) ->
                        ([(StateMachine TaggedName, Gr EnterExitState Happening)], SymbolTable)
passUniqueSymbols (sms, ust) = (sms, SymbolTable $ Map.map (fromJust . foldr1 simplifyFunc . mapJust) ust)

