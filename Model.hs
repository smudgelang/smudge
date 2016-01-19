{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}

module Model (
    EnterExitState(..),
    HappeningFlag(..),
    Happening(..),
    Identifier,
    cookWith,
    QualifiedName(..),
    mangleWith,
    SymbolType(..),
    Binding(..),
    SymbolTable,
    qName,
    passWholeStateToGraph,
    passGraphWithSymbols,
    passUniqueSymbols,
) where

import Grammars.Smudge (StateMachine(..), State(..), Event(..), SideEffect(..), StateFlag(..), WholeState)

import Prelude hiding (foldr1)
import Data.Graph.Inductive.Graph (mkGraph, Node, labNodes, labEdges)
import Data.Graph.Inductive.PatriciaTree (Gr)
import Data.Map (Map, fromList, fromListWith, unionsWith, (!))
import qualified Data.Map (map)
import Data.Set (Set, union, singleton)
import qualified Data.Set (map)
import Control.Monad (liftM)
import Data.Maybe (fromJust)
import Data.Foldable (foldr1)

data EnterExitState = EnterExitState {
        en :: [SideEffect],
        st :: State,
        ex :: [SideEffect]
    } deriving (Show, Eq, Ord)

data HappeningFlag = NoTransition
    deriving (Show, Eq, Ord)

data Happening = Happening {
        event       :: Event,
        sideEffects :: [SideEffect],
        flags       :: [HappeningFlag]
    } deriving (Show, Eq, Ord)

data Identifier = RawId String | CookedId String
    deriving (Show, Eq, Ord)

cookWith :: (String -> String) -> Identifier -> Identifier
cookWith f (RawId name) = CookedId $ f name
cookWith _ id           = id

serve :: Identifier -> String
serve (CookedId name) = name

newtype QualifiedName = QualifiedName [Identifier]
    deriving (Show, Eq, Ord)

mangleWith :: (String -> String -> String) -> (String -> String) -> QualifiedName -> String
mangleWith _  _ (QualifiedName []) = ""
mangleWith ff f q = foldr1 ff $  map (serve . cookWith f) $ (\(QualifiedName ids) -> ids) q

type Parameter = QualifiedName

type Result = QualifiedName

data SymbolType = FunctionSym Parameter Result
    deriving (Show, Eq, Ord)

data Binding = External | Unresolved | Resolved
    deriving (Show, Eq, Ord)

type UnfilteredSymbolTable = Map QualifiedName (Set (Binding, SymbolType))

type SymbolTable = Map QualifiedName (Binding, SymbolType)

smToGraph :: (StateMachine, [WholeState]) ->
                 Gr EnterExitState Happening
smToGraph (sm, ss) =
    -- Graph.mkGraph :: [(Node, node)] -> [(Node, Node, edge)] -> gr node edge
    mkGraph [s | s <- zip [1..] (EnterExitState [] StateEntry [] : map getEeState ss)] es
    where
        getEeState (st, _, en, _, ex) = EnterExitState {..}
        getState (s, _, _, _, _) = s
        sn :: Map State Node
        sn = fromList [s | s <- zip (map getState ss) [2..]]
        mkEdge :: State -> State -> Happening -> (Node, Node, Happening)
        mkEdge s s'' eses = (sn ! s, sn ! s'', eses)
        es = [ese | ese <- concat $ map f ss]
            where
                f :: WholeState -> [(Node, Node, Happening)]
                f (s, fs, _, es, _) | elem Initial fs = (1, sn ! s, Happening EventEnter [] []) : f (s, [], [], es, [])
                f (s,  _, _, es, _) = map g es
                    where
                        g :: (Event, [SideEffect], State) -> (Node, Node, Happening)
                        g (e, ses, s') =
                            let (e', s'') = case s' of
                                    StateSame -> (Happening e ses [NoTransition], s)
                                    otherwise -> (Happening e ses [], s')
                            in mkEdge s s'' e'

passWholeStateToGraph :: [(StateMachine, [WholeState])] ->
                            [(StateMachine, Gr EnterExitState Happening)]
passWholeStateToGraph sms = zip (map fst sms) (map smToGraph sms)

smName :: StateMachine -> StateMachine -> String
smName _ (StateMachine n) = n
smName (StateMachine n) _ = n
smName StateMachineSame _ = undefined

qName :: StateMachine -> SideEffect -> QualifiedName
qName _  (FuncVoid s)                 = QualifiedName [CookedId s]
qName _  (FuncEvent s _)              = QualifiedName [CookedId s]
qName sm (FuncDefault (sm', Event e)) = QualifiedName [RawId $ smName sm sm', RawId $ e]

symbols :: (StateMachine, Gr EnterExitState Happening) -> UnfilteredSymbolTable
symbols (sm, gr) =
    fromListWith union $ [(qName sm se, singleton $ (bnd se, FunctionSym (param (event h) se) (result se)))
                          | (_, _, h) <- labEdges gr, se <- sideEffects h]
                         ++ [(qName sm se, singleton $ (bnd se, FunctionSym (tparam se) (result se)))
                             | (_, EnterExitState {en, ex}) <- labNodes gr, se <- en ++ ex]
                         ++ [(qName sm $ FuncDefault (sm, e), singleton $ (Resolved, FunctionSym (qName sm $ FuncDefault (sm, e)) (QualifiedName [])))
                             | (_, _, Happening {event = e@(Event _)}) <- labEdges gr]
    where
        bnd (FuncDefault (sm', _)) | sm == sm' = Resolved
        bnd (FuncDefault _)                    = Unresolved
        bnd _                                  = External
        param _           se@(FuncDefault _) = qName sm se
        param e@(Event _) _                  = qName sm (FuncDefault (sm, e))
        param _           _                  = QualifiedName []
        tparam se@(FuncDefault _) = qName sm se
        tparam _                  = QualifiedName []
        result (FuncEvent _ qe) = qName sm (FuncDefault qe)
        result _                = QualifiedName []

passGraphWithSymbols :: [(StateMachine, Gr EnterExitState Happening)] ->
                        ([(StateMachine, Gr EnterExitState Happening)], UnfilteredSymbolTable)
passGraphWithSymbols sms = (sms, unionsWith union $ map symbols sms)

-- a result is simpler than no result
simplifyReturn :: Result -> Result -> Maybe Result
simplifyReturn a                       b              | a == b = Just a
simplifyReturn a@(QualifiedName (_:_))   (QualifiedName [])    = Just a
simplifyReturn   (QualifiedName [])    b@(QualifiedName (_:_)) = Just b
simplifyReturn _                       _                       = Nothing

-- no parameters is simpler than parameters
simplifyParam :: Parameter -> Parameter -> Parameter
simplifyParam a b | a == b = a
simplifyParam _ _          = QualifiedName []

-- an external binding cannot be resolved
resolveBinding :: Binding -> Binding -> Binding
resolveBinding a          b        | a == b = a
resolveBinding Resolved   Unresolved        = Resolved
resolveBinding Unresolved Resolved          = Resolved
resolveBinding _          _                 = undefined

simplifyFunc :: Maybe (Binding, SymbolType) -> Maybe (Binding, SymbolType) -> Maybe (Binding, SymbolType)
simplifyFunc (Just (ab, FunctionSym ap ar)) (Just (bb, FunctionSym bp br)) = liftM ((,) $ resolveBinding ab bb) (liftM (FunctionSym (simplifyParam ap bp)) (simplifyReturn ar br))
simplifyFunc _                              _                              = Nothing

mapJust :: Ord a => Set a -> Set (Maybe a)
mapJust = Data.Set.map Just

passUniqueSymbols :: ([(StateMachine, Gr EnterExitState Happening)], UnfilteredSymbolTable) ->
                        ([(StateMachine, Gr EnterExitState Happening)], SymbolTable)
passUniqueSymbols (sms, ust) = (sms, Data.Map.map (fromJust . foldr1 simplifyFunc . mapJust) ust)
