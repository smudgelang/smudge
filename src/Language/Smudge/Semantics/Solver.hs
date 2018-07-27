-- Copyright 2017 Bose Corporation.
-- This software is released under the 3-Clause BSD License.
-- The license can be viewed at https://github.com/Bose/Smudge/blob/master/LICENSE

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Language.Smudge.Semantics.Solver (
    Ty(Void, Ty, (:->)),
    Binding(..),
    resultOf,
    instantiable,

    SymbolTable,
    insertFunctions,
    toList,
    filterBind,
    (!),

    elaborateMono,
    elaboratePoly,
) where

import Language.Smudge.Grammar (
  StateMachine,
  Event(..),
  Function(..),
  SideEffect,
  EventHandler,
  WholeState
  )
import Language.Smudge.Semantics.Model (
  qualify,
  QualifiedName,
  TaggedName,
  Tagged(..),
  )
import Language.Smudge.Parsers.Id (Name)
import Language.Smudge.Passes.Passes (AbstractFoldable(..))

import Data.Map (Map, mapWithKey, foldrWithKey, unionWith, findWithDefault, member)
import qualified Data.Map as Map(map, filter, null, empty, singleton, insert, union, partition, toList, (!))
import Data.Set (Set, elems, empty, singleton, insert, union, intersection, partition, findMin, minView)
import qualified Data.Set as Set(map, filter, null, size)
import Data.List (intercalate)
import Data.Char (ord, chr)
import Control.Monad.State (State, evalState, get, put)
import Control.Monad (foldM)
import Control.Arrow (first, second, (***), (>>>))

-- external interface
data Binding = External | Unresolved | Exported | Internal
    deriving (Show, Eq, Ord)

data Ty =
        -- instantiable types
          Void
        | Ty TaggedName
        | Ty :-> Ty

        -- non-instantiable types
        | Tyvar String
        | Tyset (Set Ty)
    deriving (Eq, Ord)

instance Show Ty where
    show Void = "void"
    show (Ty n) = show $ qualify n
    show (tau :-> tau') = show tau ++ " -> " ++ show tau'
    show (Tyvar n) = n
    show (Tyset tys) = "{" ++ intercalate ", " (map show (elems tys)) ++ "}"

infixr 7 :->

resultOf :: Ty -> Ty
resultOf (_ :-> tau') = resultOf tau'
resultOf tau = tau

instantiable :: Ty -> Bool
instantiable      Void  = True
instantiable     (Ty _) = True
instantiable (t :-> t') = instantiable t && instantiable t'
instantiable         _  = False

newtype SymbolTable = SymbolTable SymTab
    deriving (Show, Eq, Ord, Semigroup, Monoid)

instance AbstractFoldable SymbolTable where
    type FoldContext SymbolTable = (TaggedName, (Binding, Ty))
    afold f a (SymbolTable gamma) = foldrWithKey (curry f) a gamma

elaborateMono :: SymbolTable -> [(StateMachine TaggedName, [(WholeState TaggedName)])] -> SymbolTable
elaborateMono (SymbolTable gamma) = SymbolTable . canonicalizeTabMono . defModule gamma

elaboratePoly :: SymbolTable -> [(StateMachine TaggedName, [(WholeState TaggedName)])] -> SymbolTable
elaboratePoly (SymbolTable gamma) = SymbolTable . canonicalizeTabPoly . defModule gamma

insertFunctions :: SymbolTable -> Binding -> [(QualifiedName, ([TaggedName], Name))] -> SymbolTable
insertFunctions (SymbolTable gamma) b fs =
    SymbolTable $ canonicalizeTabPoly $ mapWithKey ((*** instantiate theta) . rebind btheta) gamma'
    where gamma' = evalState (foldM defName gamma $ map fst fs') 0  -- BUG: 0 is probably wrong
          fs' = map (TagFunction *** funTy) fs
          funTy = map Ty *** makeTy >>> uncurry makeFun
          makeFun [] rty = Void :-> rty
          makeFun [pty] rty = pty :-> rty
          makeFun (pty:ptys) rty = pty :-> makeFun ptys rty
          makeTy "" = Void
          makeTy ty = Ty $ TagBuiltin $ qualify ty
          (btheta, theta) = solve $ conjoin [(b :@ n :/\ ty :<: gamma' !> n) | (n, ty) <- fs']

toList :: SymbolTable -> [(TaggedName, (Binding, Ty))]
toList (SymbolTable gamma) = Map.toList gamma

filterBind :: SymbolTable -> Binding -> SymbolTable
filterBind (SymbolTable gamma) b = SymbolTable $ Map.filter (\(b', _) -> b == b') gamma

(!) :: SymbolTable -> TaggedName -> (Binding, Ty)
(SymbolTable gamma) ! name = gamma Map.! name

-- internal implementation
type SymTab = Map TaggedName (Binding, Ty)

data Constraint = Ty :<: Ty
                | Binding :@ TaggedName
                | Constraint :/\ Constraint
                | Trivial
    deriving (Show, Eq, Ord)

infixl 5 :@
infixl 5 :<:
infixl 4 :/\

conjoin :: [Constraint] -> Constraint
conjoin [] = Trivial
conjoin [c] = c
conjoin (c:cs) = c :/\ conjoin cs

typefor :: TaggedName -> Ty
typefor = Ty

fresh :: State Int Ty
fresh = do n <- get
           put (n + 1)
           return $ Tyvar $ vname n
    where vname n = if   n <= ord 'z' - ord 'a'
                    then [chr $ ord 'a' + n]
                    else 'a' : show n

(!>) :: SymTab -> TaggedName -> Ty
gamma !> x = snd $ gamma Map.! x

(!?) :: SymTab -> Maybe TaggedName -> Ty
gamma !? (Just x) = gamma !> x
_ !? _ = Void

-- definition rules
defModule :: SymTab -> [(StateMachine TaggedName, [(WholeState TaggedName)])] -> SymTab
defModule gamma ms = mapWithKey ((*** instantiate theta) . rebind btheta) gammaN
    where gammaN = evalState (foldM defMachine gamma ms) 0
          c      = conjoin $ map (inferMachine gammaN) ms
          (btheta, theta) = second subst $ solve c

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
defName gamma x = if member x gamma
                     then return gamma
                     else do alpha <- fresh
                             return $ Map.insert x (Unresolved, alpha) gamma

-- constraint rules
inferMachine :: SymTab -> (StateMachine TaggedName, [(WholeState TaggedName)]) -> Constraint
inferMachine gamma (_, qs) = conjoin $ map (inferState gamma) qs

inferState :: SymTab -> WholeState TaggedName -> Constraint
inferState gamma (_, _, en, eh, ex) = c_n :/\ c_h :/\ c_x
    where c_n = inferEvent gamma (EventEnter, en, undefined)
          c_h = conjoin $ map (inferEvent gamma) eh
          c_x = inferEvent gamma (EventExit, ex, undefined)

inferEvent :: SymTab -> EventHandler TaggedName -> Constraint
inferEvent gamma (Event x_a, ds, _) = Exported :@ x_a :/\ Exported :@ x_d :/\ typefor x_a :<: tau_a :/\ c
    where retag (TagEvent n) = (TagFunction n)
          tau_a = gamma !> x_a
          x_d = retag x_a
          d_a = (x_d, FuncEvent (undefined, Event x_a))
          c = conjoin $ map (inferSE gamma (Just x_a)) (d_a:ds)
inferEvent gamma (        _, ds, _) = c
    where c = conjoin $ map (inferSE gamma Nothing ) ds

inferSE :: SymTab -> Maybe TaggedName -> SideEffect TaggedName -> Constraint
inferSE gamma x_a (x_d, f) =
    let tau = (gamma !? x_a)
        tau' = gamma !> x_d
        inferFun :: Function TaggedName -> Constraint
        inferFun FuncVoid = External :@ x_d :/\ tau :-> Void :<: tau'
        inferFun (FuncTyped (_, Event x_a')) = External :@ x_d :/\ tau :-> tau_a' :<: tau' :/\ typefor x_a' :<: tau_a'
            where tau_a' = gamma !> x_a'
        inferFun (FuncEvent (_, Event x_a')) = tau_a' :-> Void :<: tau' :/\ typefor x_a' :<: tau_a'
            where tau_a' = gamma !> x_a'
    in  inferFun f

-- subtyping rules
leastUpperBound :: Set Ty -> Set Ty
leastUpperBound = boundWith join

greatestLowerBound :: Set Ty -> Set Ty
greatestLowerBound = boundWith meet

boundWith :: (Ty -> Ty -> Ty) -> Set Ty -> Set Ty
boundWith op s =
    let isFun (_ :-> _) = True
        isFun         _ = False
    in  case first minView $ partition isFun s of
        (Nothing, s')      -> s'
        (Just (f, fs), s') -> insert (foldr op f fs) s'

join :: Ty -> Ty -> Ty
(t :-> t')   `join` (t'' :-> t''') = t'' `meet` t :-> t' `join` t'''
(Tyset taus) `join` (Tyset taus')  = Tyset $ leastUpperBound $ union taus taus'
(Tyset taus) `join` tau            = Tyset taus            `join` Tyset (singleton tau)
tau          `join` (Tyset taus)   = Tyset (singleton tau) `join` Tyset taus
tau          `join` tau'           = Tyset (singleton tau) `join` Tyset (singleton tau')

meet :: Ty -> Ty -> Ty
(Tyset taus) `meet` (Tyset taus') = Tyset $ greatestLowerBound $ intersection taus taus'
(Tyset taus) `meet` tau           = Tyset taus            `meet` Tyset (singleton tau)
tau          `meet` (Tyset taus)  = Tyset (singleton tau) `meet` Tyset taus
tau          `meet` tau'          = Tyset (singleton tau) `meet` Tyset (singleton tau')

-- unification rules

type BindSubst = Map TaggedName Binding
type TySubst = Map String Ty

(|-->) :: a -> b -> Map a b
x |--> v = Map.singleton x v
infixr 5 |-->

identity :: Map a b
identity = Map.empty

merge :: TySubst -> TySubst -> TySubst
merge = unionWith join

bmerge :: BindSubst -> BindSubst -> BindSubst
bmerge = unionWith resolveBinding

solve :: Constraint -> (BindSubst, TySubst)
solve Trivial = (identity, identity)
solve (tau :<: tau') | tau == tau' = (identity, identity)
solve (tau :<: Tyvar alpha) = (identity, alpha |--> tau)
solve (b :@ name) = (name |--> b, identity)
solve (c1 :/\ c2) = (bmerge btheta1 btheta2, merge theta1 theta2)
    where (btheta1, theta1) = solve c1
          (btheta2, theta2) = solve c2
solve c = error $ "Tried to solve '" ++ show c ++ "'.  This is a bug in smudge.\n"

-- an external binding cannot be resolved
resolveBinding :: Binding -> Binding -> Binding
resolveBinding a          b        | a == b = a
resolveBinding a          Unresolved        = a
resolveBinding Unresolved b                 = b
resolveBinding _          _                 = undefined

-- instantiation
instantiate :: TySubst -> Ty -> Ty
instantiate theta tau =
    let setof (Tyset taus) = taus
        setof tau = singleton tau
        inst (tau :-> tau')     = inst tau :-> inst tau'
        inst tau@(Tyvar alpha)  = findWithDefault tau alpha theta
        inst (Tyset taus)       = Tyset $ leastUpperBound $ foldr union empty $ Set.map (setof . inst) taus
        inst tau                = tau
    in inst tau

finished :: Ty -> Bool
finished (tau :-> tau') = finished tau && finished tau'
finished (Tyset taus) = foldr ((&&) . finished) True taus
finished tau = instantiable tau

subst :: TySubst -> TySubst
subst theta = case Map.partition finished theta of
              (theta_r, theta_u) | Map.null theta_u -> theta_r
              (theta_r, theta_u) | Map.null theta_r -> error "Unable to complete substitution.  This is a bug in smudge.\n"
              (theta_r, theta_u)                    -> Map.union theta_r (subst $ Map.map (instantiate theta_r) theta_u)

rebind :: BindSubst -> TaggedName -> Binding -> Binding
rebind theta k b = findWithDefault b k theta

-- canonicalization

canonicalizeTabMono :: SymTab -> SymTab
canonicalizeTabMono = Map.map (second canonicalize)

canonicalizeTabPoly :: SymTab -> SymTab
canonicalizeTabPoly = Map.map (second (canonicalize . bottomToVoid . voidToBottom))

canonicalize :: Ty -> Ty
canonicalize (Tyset taus) | Set.size taus == 1 = findMin taus
canonicalize (tau :-> tau') = canonicalize tau :-> canonicalize tau'
canonicalize tau = tau

voidToBottom :: Ty -> Ty
voidToBottom (Tyset taus) = Tyset $ Set.filter (/= Void) taus
voidToBottom (t :-> t') = voidToBottom t :-> voidToBottom t'
voidToBottom tau = tau

bottomToVoid :: Ty -> Ty
bottomToVoid (Tyset taus) | Set.null taus = Void
bottomToVoid (t :-> t') = bottomToVoid t :-> bottomToVoid t'
bottomToVoid tau = tau
