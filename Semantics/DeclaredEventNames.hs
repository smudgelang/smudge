{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

module Semantics.DeclaredEventNames (
    DeclaredEventNames
) where

import Grammars.Smudge (
  Event(..),
  QEvent,
  Function(..),
  SideEffect,
  WholeState,
  )
import Grammars.Smudge (StateMachine(..))
import Model (TaggedName, disqualifyTag)
import Parsers.Id (at)
import Semantics.Semantic (Passable(..), Severity(..), Fault(..))

import Data.Set (Set, fromList, toList, map, filter, (\\))
import Prelude hiding (filter, map)

data DeclaredEventNames = DeclaredEventNames (Set (Event TaggedName)) (Set (QEvent TaggedName))
instance Monoid DeclaredEventNames where
    mempty = DeclaredEventNames mempty mempty
    mappend (DeclaredEventNames eh se) (DeclaredEventNames eh' se') =
        DeclaredEventNames (mappend eh eh') (mappend se se')

qesOfSes :: [SideEffect TaggedName] -> [QEvent TaggedName]
qesOfSes ses = [qe | (_, FuncTyped qe@(_, Event _)) <- ses] ++
               [qe | (_, FuncEvent qe@(_, Event _)) <- ses]

instance Passable DeclaredEventNames where
    type Representation DeclaredEventNames = [WholeState TaggedName]
    accumulate (_, _, en, eh, ex) =
        mappend $ DeclaredEventNames (fromList [e | (e@(Event _), _, _) <- eh])
                    (fromList $ qesOfSes en ++ concat [qesOfSes ses | (_, ses, _) <- eh] ++ qesOfSes ex)
    test (sm@(StateMachine sm_name), _) (DeclaredEventNames eh se) =
        case toList ((map snd $ filter ((== sm) . fst) se) \\ eh) of
        [] -> []
        es -> [Fault ERROR (at name) $ (disqualifyTag sm_name) ++ ": Event name not declared: " ++
               disqualifyTag name | Event name <- es]
