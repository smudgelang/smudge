{-# LANGUAGE NamedFieldPuns #-}

module Semantics.Solver (
    insertExternalSymbol,
    SymbolType(..),
    Binding(..),
    SymbolTable,
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
import qualified Data.Map (map)
import Data.Set (Set, union, singleton)
import qualified Data.Set (map)
import Control.Monad (liftM)
import Data.Maybe (fromJust)

data Binding = External | Unresolved | Resolved
    deriving (Show, Eq, Ord)

type Parameter = TaggedName

type Result = TaggedName

data SymbolType = FunctionSym Parameter Result
    deriving (Show, Eq, Ord)

type UnfilteredSymbolTable = Map TaggedName (Set (Binding, SymbolType))

type SymbolTable = Map TaggedName (Binding, SymbolType)

-- Old table, Name, args, return type, new table
insertExternalSymbol :: SymbolTable -> Name -> [Name] -> Name -> SymbolTable
insertExternalSymbol table fname args returnType = Data.Map.insertWith
    simplifyDefinitely
    (TagFunction, QualifiedName [CookedId fname])
    (External, FunctionSym (TagBuiltin, QualifiedName (map CookedId args))
                           (TagBuiltin, QualifiedName [CookedId returnType]))
    table

symbols :: (StateMachine TaggedName, Gr EnterExitState Happening) -> UnfilteredSymbolTable
symbols (Annotated _ sm, gr) =
    fromListWith union $ [(n, singleton $ (bnd f, FunctionSym (param (event h) se) (result se)))
                          | (_, _, h) <- labEdges gr, se@(n, f) <- sideEffects h]
                         ++ [(n, singleton $ (bnd f, FunctionSym (tparam se) (result se)))
                             | (_, EnterExitState {en, ex}) <- labNodes gr, se@(n, f) <- en ++ ex]
                         ++ [(e, singleton $ (Resolved, FunctionSym e (TagBuiltin, QualifiedName [])))
                             | (_, _, Happening {event = (Event e)}) <- labEdges gr]
    where
        bnd (FuncEvent (sm', _)) | sm == sm' = Resolved
        bnd (FuncEvent _)                    = Unresolved
        bnd _                                = External
        param _           (_, FuncEvent (_, (Event e))) = e
        param _           (_, FuncEvent (_, _)) = undefined
        param (Event e)   _                     = e
        param _           _                     = (TagBuiltin, QualifiedName [])
        tparam (_, FuncEvent (_, (Event e))) = e
        tparam (_, FuncEvent (_, _)) = undefined
        tparam _                     = (TagBuiltin, QualifiedName [])
        result (_, FuncTyped (_, (Event e))) = e
        result (_, FuncTyped (_, _)) = undefined
        result _                     = (TagBuiltin, QualifiedName [])

passGraphWithSymbols :: [(StateMachine TaggedName, Gr EnterExitState Happening)] ->
                        ([(StateMachine TaggedName, Gr EnterExitState Happening)], UnfilteredSymbolTable)
passGraphWithSymbols sms = (sms, unionsWith union $ map symbols sms)

-- a result is simpler than no result
simplifyReturn :: Result -> Result -> Maybe Result
simplifyReturn a                            b              | a == b  = Just a
simplifyReturn a@(_, QualifiedName (_:_))   (_, QualifiedName [])    = Just a
simplifyReturn   (_, QualifiedName [])    b@(_, QualifiedName (_:_)) = Just b
simplifyReturn _                            _                        = Nothing

-- no parameters is simpler than parameters
simplifyParam :: Parameter -> Parameter -> Parameter
simplifyParam a b | a == b = a
simplifyParam _ _          = (TagBuiltin, QualifiedName [])

-- an external binding cannot be resolved
resolveBinding :: Binding -> Binding -> Binding
resolveBinding a          b        | a == b = a
resolveBinding Resolved   Unresolved        = Resolved
resolveBinding Unresolved Resolved          = Resolved
resolveBinding _          _                 = undefined

simplifyFunc :: Maybe (Binding, SymbolType) -> Maybe (Binding, SymbolType) -> Maybe (Binding, SymbolType)
simplifyFunc (Just (ab, FunctionSym ap ar)) (Just (bb, FunctionSym bp br)) = liftM ((,) $ resolveBinding ab bb) (liftM (FunctionSym (simplifyParam ap bp)) (simplifyReturn ar br))
simplifyFunc _                              _                              = Nothing

simplifyDefinitely :: (Binding, SymbolType) -> (Binding, SymbolType) -> (Binding, SymbolType)
simplifyDefinitely a b = fromJust (simplifyFunc (Just a) (Just b))

mapJust :: Ord a => Set a -> Set (Maybe a)
mapJust = Data.Set.map Just

passUniqueSymbols :: ([(StateMachine TaggedName, Gr EnterExitState Happening)], UnfilteredSymbolTable) ->
                        ([(StateMachine TaggedName, Gr EnterExitState Happening)], SymbolTable)
passUniqueSymbols (sms, ust) = (sms, Data.Map.map (fromJust . foldr1 simplifyFunc . mapJust) ust)
