{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Semantics.Solver (
    Ty(Void, Ty, Unary),
    Binding(..),

    SymbolTable,
    insertExternalSymbol,
    toList,
    (!),

    elaborateMono,
    elaboratePoly,
) where

import Grammars.Smudge (
  Name,
  StateMachine,
  Event(..),
  Function(..),
  SideEffect,
  EventHandler,
  WholeState
  )
import Model (
  qualify,
  TaggedName(..),
  )

import Data.Map (Map, unionWith, findWithDefault, member)
import qualified Data.Map as Map(map, null, empty, singleton, insert, union, partition, toList, (!))
import Data.Set (Set, empty, singleton, insert, union, intersection, partition, findMin, minView)
import qualified Data.Set as Set(map, filter, null, size)
import Data.Graph.Inductive.Query.Monad (mapFst)
import Data.Char (ord, chr)
import Control.Monad.State (State, evalState, get, put)
import Control.Monad (foldM)

-- external interface
data Binding = External | Unresolved | Resolved
    deriving (Show, Eq, Ord)

data Ty =
        -- instantiable types
          Void
        | Ty Binding TaggedName
        | Unary Binding Ty Ty

        -- non-instantiable types
        | Tyvar String
        | Tyset (Set Ty)
    deriving (Show, Eq, Ord)

newtype SymbolTable = SymbolTable SymTab
    deriving (Show, Eq, Ord, Monoid)

elaborateMono :: [(StateMachine TaggedName, [(WholeState TaggedName)])] -> SymbolTable
elaborateMono = SymbolTable . Map.map  canonicalize . defModule Map.empty

elaboratePoly :: [(StateMachine TaggedName, [(WholeState TaggedName)])] -> SymbolTable
elaboratePoly = SymbolTable . Map.map (canonicalize . bottomToVoid . voidToBottom) . defModule Map.empty

insertExternalSymbol :: SymbolTable -> Name -> [Name] -> Name -> SymbolTable
insertExternalSymbol (SymbolTable gamma) fname args returnType = SymbolTable $ Map.map (instantiate theta) gamma'
    where gamma' = evalState (defName gamma name) 0  -- BUG: 0 is probably wrong
          name = (TagFunction $ qualify fname)
          ty = (Unary External
                      -- TODO: args is variable arity, but for now only unary is possible.
                      (if null args then Void else Ty External (TagBuiltin $ qualify $ head args))
                      (if null returnType then Void else Ty External (TagBuiltin $ qualify returnType)))
          theta = solve (ty :<: gamma' Map.! name)

toList :: SymbolTable -> [(TaggedName, Ty)]
toList (SymbolTable gamma) = Map.toList gamma

(!) :: SymbolTable -> TaggedName -> Ty
(SymbolTable gamma) ! name = gamma Map.! name

-- internal implementation
type SymTab = Map TaggedName Ty

data Constraint = Ty :<: Ty
                | Constraint :/\ Constraint
                | Trivial
    deriving (Show, Eq, Ord)

infixl 5 :<:
infixl 4 :/\

conjoin :: [Constraint] -> Constraint
conjoin [] = Trivial
conjoin [c] = c
conjoin (c:cs) = c :/\ conjoin cs

typefor :: TaggedName -> Ty
typefor = Ty Resolved

unknown :: TaggedName -> Ty
unknown = Ty Unresolved

fresh :: State Int Ty
fresh = do n <- get
           put (n + 1)
           return $ Tyvar $ vname n
    where vname n = if   n <= ord 'z' - ord 'a'
                    then [chr $ ord 'a' + n]
                    else 'a' : show n

-- definition rules
defModule :: SymTab -> [(StateMachine TaggedName, [(WholeState TaggedName)])] -> SymTab
defModule gamma ms = Map.map (instantiate theta) gammaN
    where gammaN = evalState (foldM defMachine gamma ms) 0
          c      = conjoin $ map (inferMachine gammaN) ms
          theta  = subst $ solve c

defMachine :: SymTab -> (StateMachine TaggedName, [(WholeState TaggedName)]) -> State Int SymTab
defMachine gamma (_, qs) = foldM defState gamma qs

defState :: SymTab -> WholeState TaggedName -> State Int SymTab
defState gamma (_, _, en, eh, ex) =
    do gamma'   <-       defEvent gamma   (EventEnter, en, undefined)
       gamma''  <- foldM defEvent gamma'  eh
       gamma''' <-       defEvent gamma'' (EventExit,  ex, undefined)
       return gamma'''

defEvent :: SymTab -> EventHandler TaggedName -> State Int SymTab
defEvent gamma (Event x_a, ds, _) =
    do gamma'   <- defName gamma x_a
       gamma''  <- defName gamma' $ retag x_a
       gamma''' <- foldM defSE gamma'' ds
       return gamma'''
    where retag (TagEvent n) = (TagFunction n)
defEvent gamma (_, ds, _) = foldM defSE gamma ds

defSE :: SymTab -> SideEffect TaggedName -> State Int SymTab
defSE gamma (x_d, f) =
    do gamma' <- defName gamma x_d
       gamma'' <- defFun gamma' f
       return gamma''

defFun :: SymTab -> Function TaggedName -> State Int SymTab
defFun gamma FuncVoid = return gamma
defFun gamma (FuncTyped (_, Event x_a')) = defName gamma x_a'
defFun gamma (FuncEvent (_, Event x_a')) = defName gamma x_a'

defName :: SymTab -> TaggedName -> State Int SymTab
defName gamma x = do if member x gamma
                        then return gamma
                        else do alpha <- fresh
                                return $ Map.insert x alpha gamma

-- constraint rules
inferMachine :: SymTab -> (StateMachine TaggedName, [(WholeState TaggedName)]) -> Constraint
inferMachine gamma (_, qs) = conjoin $ map (inferState gamma) qs

inferState :: SymTab -> WholeState TaggedName -> Constraint
inferState gamma (_, _, en, eh, ex) = c_n :/\ c_h :/\ c_x
    where c_n = inferEvent gamma (EventEnter, en, undefined)
          c_h = conjoin $ map (inferEvent gamma) eh
          c_x = inferEvent gamma (EventExit, ex, undefined)

inferEvent :: SymTab -> EventHandler TaggedName -> Constraint
inferEvent gamma (Event x_a, ds, _) = typefor x_a :<: gamma Map.! x_a :/\ c
    where retag (TagEvent n) = (TagFunction n)
          d_a = (retag x_a, FuncEvent (undefined, Event x_a))
          c = conjoin $ map (inferSE gamma (Just x_a)) (d_a:ds)
inferEvent gamma (        _, ds, _) = c
    where c = conjoin $ map (inferSE gamma Nothing ) ds

(!?) :: SymTab -> Maybe TaggedName -> Ty
gamma !? (Just a) = gamma Map.! a
_ !? _ = Void

inferSE :: SymTab -> Maybe TaggedName -> SideEffect TaggedName -> Constraint
inferSE gamma x_a (x_d, f) =
    let tau = (gamma !? x_a)
        tau' = gamma Map.! x_d
        inferFun :: Function TaggedName -> Constraint
        inferFun FuncVoid = Unary External tau Void :<: tau'
        inferFun (FuncTyped (_, Event x_a')) = Unary External tau tau_a' :<: tau' :/\ unknown x_a' :<: tau_a'
            where tau_a' = (gamma Map.! x_a')
        inferFun (FuncEvent (_, Event x_a')) = Unary Unresolved tau_a' Void :<: tau' :/\ unknown x_a' :<: tau_a'
            where tau_a' = (gamma Map.! x_a')
    in  inferFun f

-- subtyping rules
leastUpperBound :: Set Ty -> Set Ty
leastUpperBound = boundWith join

greatestLowerBound :: Set Ty -> Set Ty
greatestLowerBound = boundWith meet

boundWith :: (Ty -> Ty -> Ty) -> Set Ty -> Set Ty
boundWith op s =
    let isFun (Unary _ _ _) = True
        isFun             _ = False
        eqName n (Ty _ n') = n == n'
        eqName _ _ = False
        resolveTy (Ty b n) (Ty b' n') = Ty (resolveBinding b b') n
        rTy t@(Ty b n) ts = case partition (eqName n) ts of
                            (ts', ts'') -> insert (foldr resolveTy t ts') ts''
        rTy t          ts = insert t ts
        resolveAll s = foldr rTy empty s
    in  case mapFst minView $ partition isFun s of
        (Nothing, s')      -> resolveAll s'
        (Just (f, fs), s') -> insert (foldr op f fs) (resolveAll s')

join :: Ty -> Ty -> Ty
(Unary b t t') `join` (Unary b' t'' t''') = Unary (resolveBinding b b') (t'' `meet` t) (t' `join` t''')
(Tyset taus)   `join` (Tyset taus')       = Tyset $ leastUpperBound $ union taus taus'
(Tyset taus)   `join` tau                 = Tyset taus            `join` Tyset (singleton tau)
tau            `join` (Tyset taus)        = Tyset (singleton tau) `join` Tyset taus
tau            `join` tau'                = Tyset (singleton tau) `join` Tyset (singleton tau')

meet :: Ty -> Ty -> Ty
(Tyset taus) `meet` (Tyset taus') = Tyset $ greatestLowerBound $ intersection taus taus'
(Tyset taus) `meet` tau           = Tyset taus            `meet` Tyset (singleton tau)
tau          `meet` (Tyset taus)  = Tyset (singleton tau) `meet` Tyset taus
tau          `meet` tau'          = Tyset (singleton tau) `meet` Tyset (singleton tau')

-- an external binding cannot be resolved
resolveBinding :: Binding -> Binding -> Binding
resolveBinding a          b        | a == b = a
resolveBinding Resolved   Unresolved        = Resolved
resolveBinding Unresolved Resolved          = Resolved
resolveBinding _          _                 = undefined

-- unification rules

type TySubst = Map String Ty

(|-->) :: String -> Ty -> TySubst
tau |--> theta = Map.singleton tau theta
infixr 5 |-->

identity :: TySubst
identity = Map.empty

merge :: TySubst -> TySubst -> TySubst
merge = unionWith join

solve :: Constraint -> TySubst
solve Trivial = identity
solve (tau :<: tau') | tau == tau' = identity
solve (tau :<: Tyvar alpha) = alpha |--> tau
solve (c1 :/\ c2) = merge theta1 theta2
    where theta1 = solve c1
          theta2 = solve c2
solve c = error $ "Tried to solve '" ++ show c ++ "'.  This is a bug in smudge.\n"

-- instantiation
instantiate :: TySubst -> Ty -> Ty
instantiate theta tau =
    let setof (Tyset taus) = taus
        setof tau = singleton tau
        inst (Unary b tau tau') = Unary b (inst tau) (inst tau')
        inst tau@(Tyvar alpha)  = findWithDefault tau alpha theta
        inst (Tyset taus)       = Tyset $ leastUpperBound $ foldr union empty $ Set.map (setof . inst) taus
        inst tau                = tau
    in inst tau

finished :: Ty -> Bool
finished Void = True
finished (Ty _ _) = True
finished (Unary _ tau tau') = finished tau && finished tau'
finished (Tyvar _) = False
finished (Tyset taus) = foldr ((&&) . finished) True taus

subst :: TySubst -> TySubst
subst theta = case Map.partition finished theta of
              (theta_r, theta_u) | Map.null theta_u -> theta_r
              (theta_r, theta_u) | Map.null theta_r -> error "Unable to complete substitution.  This is a bug in smudge.\n"
              (theta_r, theta_u)                    -> Map.union theta_r (subst $ Map.map (instantiate theta_r) theta_u)


-- canonicalization
canonicalize :: Ty -> Ty
canonicalize (Tyset taus) | Set.size taus == 1 = findMin taus
canonicalize (Unary b tau tau') = Unary (rb b tau'') tau'' (canonicalize tau')
    where tau'' = canonicalize tau
          rb Unresolved (Ty Resolved _) = Resolved
          rb b _ = b
canonicalize tau = tau

voidToBottom :: Ty -> Ty
voidToBottom (Tyset taus) = Tyset $ Set.filter (/= Void) taus
voidToBottom (Unary b t t') = Unary b (voidToBottom t) (voidToBottom t')
voidToBottom tau = tau

bottomToVoid :: Ty -> Ty
bottomToVoid (Tyset taus) | Set.null taus = Void
bottomToVoid (Unary b t t') = Unary b (bottomToVoid t) (bottomToVoid t')
bottomToVoid tau = tau
