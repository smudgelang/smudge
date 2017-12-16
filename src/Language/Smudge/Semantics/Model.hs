-- Copyright 2017 Bose Corporation.
-- This software is released under the 3-Clause BSD License.
-- The license can be viewed at https://github.com/Bose/Smudge/blob/master/LICENSE

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

module Language.Smudge.Semantics.Model (
    EnterExitState(..),
    HappeningFlag(..),
    Happening(..),
    QualifiedName,
    Qualifiable(..),
    extractWith,
    mangleWith,
    disqualify,
    disqualifyTag,
    events_for,
    TaggedName,
    Tagged(..),
    qName,
    passInitialState,
    passFullyQualify,
    passRename,
    passTagCategories,
    passWholeStateToGraph,
) where

import Language.Smudge.Grammar (
  StateMachine(..),
  State(..),
  Event(..),
  QEvent,
  Function(..),
  SideEffect(..),
  StateFlag(..),
  WholeState
  )
import Language.Smudge.Parsers.Id (
  Name,
  Declared(..),
  Identifier,
  mangle,
  )
import Language.Smudge.Semantics.Alias (Alias, rename)

import Data.Graph.Inductive.Graph (mkGraph, labEdges, Node)
import Data.Graph.Inductive.PatriciaTree (Gr)
import Data.Map (Map, fromList, (!))
import Data.List (intercalate, nub, sort)
import Data.List.NonEmpty (NonEmpty((:|)), toList, (<|))
import Data.Semigroup (Semigroup, (<>))
import Control.Arrow (first)

data EnterExitState = EnterExitState {
        en :: [SideEffect TaggedName],
        st :: State TaggedName,
        ex :: [SideEffect TaggedName]
    } deriving (Show, Eq, Ord)

data HappeningFlag = NoTransition
    deriving (Show, Eq, Ord)

data Happening = Happening {
        event       :: Event TaggedName,
        sideEffects :: [SideEffect TaggedName],
        flags       :: [HappeningFlag]
    } deriving (Show, Eq, Ord)

newtype Qualified a = Qualified (NonEmpty a)
    deriving (Eq, Ord, Semigroup, Functor, Applicative, Monad)

instance Show a => Show (Qualified a) where
    show (Qualified as) = intercalate "." $ toList $ fmap show as

instance Read a => Read (Qualified a) where
    readsPrec d = readParen False
                    (\r -> fmap (first Qualified) $ readA r)
        where readA r = do
                    (a, s) <- readsPrec d r
                    case s of
                        '.' : s -> fmap (first (a <|)) $ readA s
                        otherwise -> [(a :| [], s)]

instance Declared a => Declared (Qualified a) where
    at = extractWith seq at

type QualifiedName = Qualified Identifier

class Qualifiable n where
    qualify :: n -> QualifiedName

instance Qualifiable QualifiedName where
    qualify = id

instance Qualifiable Identifier where
    qualify n = Qualified (n :| [])

instance Qualifiable Name where
    qualify = read . ('@':)

instance (Qualifiable s, Qualifiable n) => Qualifiable (s, n) where
    qualify (s, n) = qualify s <> qualify n

extractWith :: (a -> a -> a) -> (b -> a) -> Qualified b -> a
extractWith ff f (Qualified ns) = foldr1 ff $ fmap f ns

mangleWith :: (Name -> Name -> Name) -> (Name -> Name) -> QualifiedName -> Name
mangleWith ff f = extractWith ff (mangle f)

disqualify :: QualifiedName -> Name
disqualify = mangleWith seq id

data Tagged a =
          TagMachine a
        | TagState a
        | TagEvent a
        | TagFunction a
        | TagBuiltin a
    deriving (Show, Eq, Ord)

instance Functor Tagged where
    fmap f (TagMachine x) = TagMachine $ f x
    fmap f (TagState x) = TagState $ f x
    fmap f (TagEvent x) = TagEvent $ f x
    fmap f (TagFunction x) = TagFunction $ f x
    fmap f (TagBuiltin x) = TagBuiltin $ f x

instance Foldable Tagged where
    foldMap f (TagMachine x) = f x
    foldMap f (TagState x) = f x
    foldMap f (TagEvent x) = f x
    foldMap f (TagFunction x) = f x
    foldMap f (TagBuiltin x) = f x

instance Traversable Tagged where
    traverse f (TagMachine x) = TagMachine <$> f x
    traverse f (TagState x) = TagState <$> f x
    traverse f (TagEvent x) = TagEvent <$> f x
    traverse f (TagFunction x) = TagFunction <$> f x
    traverse f (TagBuiltin x) = TagBuiltin <$> f x

type TaggedName = Tagged QualifiedName

instance Declared a => Declared (Tagged a) where
    at (TagMachine n) = at n
    at (TagState n) = at n
    at (TagEvent n) = at n
    at (TagFunction n) = at n
    at (TagBuiltin n) = at n

instance Qualifiable a => Qualifiable (Tagged a) where
    qualify (TagMachine n) = qualify n
    qualify (TagState n) = qualify n
    qualify (TagEvent n) = qualify n
    qualify (TagFunction n) = qualify n
    qualify (TagBuiltin n) = qualify n

disqualifyTag :: TaggedName -> Name
disqualifyTag = disqualify . qualify

events_for :: Gr EnterExitState Happening -> [Event TaggedName]
events_for g = nub $ sort [e | (_, _, Happening {event=e@(Event _)}) <- labEdges g]

passInitialState :: [(StateMachine Identifier, [WholeState Identifier])] -> [(StateMachine Identifier, [WholeState Identifier])]
passInitialState sms = map (\(sm, wss) -> (sm, foldr init [] wss)) sms
    where init ws@(s, fs, en, es, ex) wss | elem Initial fs = (StateEntry, [], [], [(EventEnter, [], s)], []) : (s, filter (/= Initial) fs, en, es, ex) : wss
          init ws wss = ws : wss

pickSm :: StateMachine a -> StateMachine a -> StateMachine a
pickSm _ s@(StateMachine _) = s
pickSm s@(StateMachine _) _ = s
pickSm StateMachineSame _ = undefined

instance Qualifiable (StateMachine Identifier) where
    qualify (StateMachine n) = qualify n
    qualify StateMachineSame           = undefined

instance Qualifiable (Event Identifier) where
    qualify (Event e) = qualify e
    qualify _         = undefined

qQE :: StateMachine Identifier -> QEvent Identifier -> QualifiedName
qQE sm (sm', ev) = qualify (pickSm sm sm', ev)

qName :: StateMachine Identifier -> SideEffect Identifier -> QualifiedName
qName _  (s, FuncVoid)    = qualify s
qName _  (s, FuncTyped _) = qualify s
qName sm (_, FuncEvent e) = qQE sm e

passFullyQualify :: [(StateMachine Identifier, [WholeState Identifier])] -> [(StateMachine QualifiedName, [WholeState QualifiedName])]
passFullyQualify sms = map qual sms
    where qual (sm, wss) = (qual_sm sm, map qual_ws wss)
            where qual_sm = fmap qualify
                  qual_ws (st, fs, en, es, ex) = (qual_st st, fs, map qual_fn en, map qual_eh es, map qual_fn ex)
                  qual_eh (ev, ses, s) = (qual_ev ev, map qual_fn ses, qual_st s)
                  qual_st = fmap (qualify . ((,) sm))
                  qual_ev = fmap (qualify . ((,) sm))
                  qual_qe ev@(sm', _) = (qual_sm $ pickSm sm sm', Event $ qQE sm ev)
                  qual_fn fn@(_, FuncVoid)     = (qName sm fn, FuncVoid)
                  qual_fn fn@(_, FuncTyped qe) = (qName sm fn, FuncTyped $ qual_qe qe)
                  qual_fn fn@(_, FuncEvent qe) = (qName sm fn, FuncEvent $ qual_qe qe)

passRename :: Alias QualifiedName -> (QualifiedName -> QualifiedName) -> [(StateMachine QualifiedName, [WholeState QualifiedName])] -> [(StateMachine QualifiedName, [WholeState QualifiedName])]
passRename aliases nsprefix sms = map ren sms
    where rename' = rename aliases . nsprefix
          ren (sm, wss) = (fmap rename' sm, map ren_ws wss)
          ren_ws (st, fs, en, es, ex) = (fmap rename' st, fs, map ren_fn en, map ren_eh es, map ren_fn ex)
          ren_eh (ev, ses, s) = (fmap rename' ev, map ren_fn ses, fmap rename' s)
          ren_qe (sm, ev) = (fmap rename' sm, fmap rename' ev)
          ren_fn (n, FuncVoid)     = (rename' n, FuncVoid)
          ren_fn (n, FuncTyped qe) = (rename' n, FuncTyped $ ren_qe qe)
          ren_fn (n, FuncEvent qe) = (rename' n, FuncEvent $ ren_qe qe)

passTagCategories :: [(StateMachine QualifiedName, [WholeState QualifiedName])] -> [(StateMachine TaggedName, [WholeState TaggedName])]
passTagCategories sms = map tag sms
    where tag (sm, wss) = (fmap TagMachine sm, map tag_ws wss)
          tag_ws (st, fs, en, es, ex) = (fmap TagState st, fs, map tag_fn en, map tag_eh es, map tag_fn ex)
          tag_eh (ev, ses, s) = (fmap TagEvent ev, map tag_fn ses, fmap TagState s)
          tag_qe (sm', ev) = (fmap TagMachine sm', fmap TagEvent ev)
          tag_fn (n, FuncVoid)     = (TagFunction n, FuncVoid)
          tag_fn (n, FuncTyped qe) = (TagFunction n, FuncTyped $ tag_qe qe)
          tag_fn (n, FuncEvent qe) = (TagFunction n, FuncEvent $ tag_qe qe)

smToGraph :: (StateMachine TaggedName, [WholeState TaggedName]) ->
                 Gr EnterExitState Happening
smToGraph (sm, ss) = mkGraph eess es
    where
        ss' = zip [1..] ss
        getEeState (n, (st, _, en, _, ex)) = (n, EnterExitState {..})
        eess = map getEeState ss'
        sn :: Map (State TaggedName) Node
        sn = fromList $ map (\(n, ees) -> (st ees, n)) eess
        mkEdge :: Node -> (State TaggedName) -> Happening -> (Node, Node, Happening)
        mkEdge n s'' eses = (n, sn ! s'', eses)
        es = [ese | ese <- concat $ map (\(n, (s, _, _, es, _)) -> map (g n s) es) ss']
        g :: Node -> (State TaggedName) -> (Event TaggedName, [SideEffect TaggedName], State TaggedName) -> (Node, Node, Happening)
        g n s (e, ses, s') =
            let (e', s'') = case s' of
                    StateSame -> (Happening e ses [NoTransition], s)
                    otherwise -> (Happening e ses [], s')
            in mkEdge n s'' e'

passWholeStateToGraph :: [(StateMachine TaggedName, [WholeState TaggedName])] ->
                            [(StateMachine TaggedName, Gr EnterExitState Happening)]
passWholeStateToGraph sms = zip (map fst sms) (map smToGraph sms)
