{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}

module Semantics.Solver (
    Ty(..),
    Binding(..),

    SymbolTable,
    insertExternalSymbol,
    toList,
    (!),

    elaboratePoly,
) where

import Grammars.Smudge (
  Name,
  Annotated(..),
  StateMachine(..),
  Event(..),
  Function(..),
  EventHandler,
  WholeState
  )
import Model (
  qualify,
  TaggedName(..),
  EnterExitState(..),
  Happening(..),
  )

import Data.Map (Map, empty, insert, fromListWith, unionsWith, insertWith, foldrWithKey)
import qualified Data.Map as Map(map, lookup, toList, (!))
import Control.Monad.State (State, evalState)
import Control.Monad (foldM)

-- external interface
data Binding = External | Unresolved | Resolved
    deriving (Show, Eq, Ord)

data Ty = Void
        | Ty Binding TaggedName
        | Unary Binding Ty Ty
    deriving (Show, Eq, Ord)

newtype SymbolTable = SymbolTable SymTab
    deriving (Show, Eq, Ord, Monoid)

elaboratePoly :: [(StateMachine TaggedName, [(WholeState TaggedName)])] -> SymbolTable
elaboratePoly = SymbolTable . defModule empty

-- Old table, Name, args, return type, new table
insertExternalSymbol :: SymbolTable -> Name -> [Name] -> Name -> SymbolTable
insertExternalSymbol (SymbolTable gamma) fname args returnType = SymbolTable $ Data.Map.insertWith simplifyFunc name ty gamma
    where
          name = (TagFunction $ qualify fname)
          ty = (Unary External
                      -- TODO: args is variable arity, but for now only unary is possible.
                      (if null args then Void else Ty External (TagBuiltin $ qualify $ head args))
                      (if null returnType then Void else Ty External (TagBuiltin $ qualify returnType)))

toList :: SymbolTable -> [(TaggedName, Ty)]
toList (SymbolTable gamma) = Map.toList gamma

(!) :: SymbolTable -> TaggedName -> Ty
(SymbolTable gamma) ! name = gamma Map.! name

-- internal implementation
type SymTab = Map TaggedName Ty

typefor :: TaggedName -> Ty
typefor = Ty Resolved

-- definition rules
defModule :: SymTab -> [(StateMachine TaggedName, [(WholeState TaggedName)])] -> SymTab
defModule gamma ms = resolve gammaN
    where gammaN = evalState (foldM defMachine gamma ms) 0

defMachine :: SymTab -> (StateMachine TaggedName, [(WholeState TaggedName)]) -> State Int SymTab
defMachine gamma (_, qs) = foldM defState gamma qs

defState :: SymTab -> WholeState TaggedName -> State Int SymTab
defState gamma (_, _, en, eh, ex) =
    do gamma'   <- foldM (defName Void)  gamma   en
       gamma''  <- foldM defEvent gamma'  eh
       gamma''' <- foldM (defName Void)  gamma'' ex
       return gamma'''

defEvent :: SymTab -> EventHandler TaggedName -> State Int SymTab
defEvent gamma (ev, ds, _) = foldM (defName (tyof ev)) gamma' fs
    where retag (TagEvent n) = (TagFunction n)
          tyof (Event a) = typefor a
          tyof         _ = Void
          fs = [(retag a, FuncEvent (undefined, Event a)) | (Event a) <- [ev]] ++ ds
          gamma' = foldl (\g x -> insert x (typefor x) g) gamma [a | (Event a) <- [ev]]

defName :: Ty -> SymTab -> (TaggedName, Function TaggedName) -> State Int SymTab
defName ty gamma (x, f) = do let ty' = case (Map.lookup x gamma) of
                                       Just ty' -> (simplifyFunc (funTy ty f) ty')
                                       Nothing -> (funTy ty f)
                             return $ insert x ty' gamma

funTy :: Ty -> Function TaggedName -> Ty
funTy ty FuncVoid = Unary External ty Void
funTy ty (FuncTyped (_, Event a')) = Unary External ty (Ty Unresolved a')
funTy _  (FuncEvent (_, Event a')) = Unary Unresolved (Ty Unresolved a') Void

-- a result is simpler than no result
simplifyReturn :: Ty -> Ty -> Ty
simplifyReturn a    b | a == b = a
simplifyReturn a    Void       = a
simplifyReturn Void b          = b
simplifyReturn _    _          = Void

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

simplifyFunc :: Ty -> Ty -> Ty
simplifyFunc (Unary ab ap ar) (Unary bb bp br) = Unary (resolveBinding ab bb) (simplifyParam ap bp) (simplifyReturn ar br)
simplifyFunc _                _                = undefined

resolve :: SymTab -> SymTab
resolve gamma = Map.map (\ty -> rty ty) gamma
    where bindings = foldrWithKey findAllBindings mempty gamma
          findAllBindings n ts rs = unionsWith resolveBinding [findSomeBindings n ts, rs]
          findSomeBindings n ty = fromListWith resolveBinding (bn n ty)
          rty Void           = Void
          rty (Ty _ n)       = Ty (bindings Map.! n) n
          rty (Unary b t t') = Unary (rb b (rty t)) (rty t) (rty t')
          rb Unresolved (Ty Resolved _) = Resolved
          rb b _ = b
          bty Void           = []
          bty (Ty b n)       = [(n, b)]
          bty (Unary _ t t') = bty t ++ bty t'
          bn _ Void             = []
          bn n ty@(Ty b _)      = [(n, b)] ++ bty ty
          bn n ty@(Unary b _ _) = [(n, b)] ++ bty ty
