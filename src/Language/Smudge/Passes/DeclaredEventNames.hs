-- Copyright 2018 Bose Corporation.
-- This software is released under the 3-Clause BSD License.
-- The license can be viewed at https://github.com/Bose/Smudge/blob/master/LICENSE

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

module Language.Smudge.Passes.DeclaredEventNames (
    DeclaredEventNames
) where

import Language.Smudge.Grammar (
  StateMachine(..),
  Event(..),
  QEvent,
  Function(..),
  SideEffect,
  WholeState,
  )
import Language.Smudge.Semantics.Model (TaggedName, disqualifyTag)
import Language.Smudge.Parsers.Id (at)
import Language.Smudge.Passes.Passes (Passable(..), Severity(..), Fault(..))

import Data.Set (Set, fromList, toList, map, (\\))
import Prelude hiding (map)

data DeclaredEventNames = DeclaredEventNames (Set (QEvent TaggedName)) (Set (QEvent TaggedName))
instance Monoid DeclaredEventNames where
    mempty = DeclaredEventNames mempty mempty
    mappend (DeclaredEventNames eh se) (DeclaredEventNames eh' se') =
        DeclaredEventNames (mappend eh eh') (mappend se se')

qesOfSes :: [SideEffect TaggedName] -> [QEvent TaggedName]
qesOfSes ses = [qe | (_, FuncTyped qe@(_, Event _)) <- ses] ++
               [qe | (_, FuncEvent qe@(_, Event _)) <- ses]

instance Passable DeclaredEventNames where
    type Representation DeclaredEventNames = [(StateMachine TaggedName, [WholeState TaggedName])]
    accumulate (sm, wss) = mappend $ foldr go mempty wss
        where go (_, _, en, eh, ex) =
                mappend $ DeclaredEventNames (fromList [(sm, e) | (e@(Event _), _, _) <- eh])
                    (fromList $ qesOfSes en ++ concat [qesOfSes ses | (_, ses, _) <- eh] ++ qesOfSes ex)
    test _ (DeclaredEventNames eh se) =
        case toList (se \\ eh) of
        [] -> []
        es -> [Fault ERROR (at name) $ (disqualifyTag sm_name) ++ ": Event name not declared: " ++
               disqualifyTag name | (StateMachine sm_name, Event name) <- es]
