-- Copyright 2018 Bose Corporation.
-- This software is released under the 3-Clause BSD License.
-- The license can be viewed at https://github.com/Bose/Smudge/blob/master/LICENSE

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

module Language.Smudge.Passes.OneEventHandlerPerState (
    OneEventHandlerPerState
) where

import Language.Smudge.Grammar (StateMachine(..), State(..), Event(..))
import Language.Smudge.Semantics.Model (EnterExitState(st), Happening(event), TaggedName, disqualifyTag)
import Language.Smudge.Parsers.Id (at)
import Language.Smudge.Passes.Passes (Passable(..), Severity(..), Fault(..))

import Control.Arrow ((***))
import Data.Graph.Inductive.Graph (Graph, Node, lab)
import Data.Maybe (fromJust)
import Data.Monoid (Monoid(..))
import Data.Map.Monoidal (MonoidalMap)
import Data.Tuple (swap)
import Data.List (sort, group)
import GHC.Exts (fromList, toList)

data (Graph gr) => OneEventHandlerPerState gr = OneEventHandlerPerState (MonoidalMap Node [Event TaggedName])
instance (Graph gr) => Semigroup (OneEventHandlerPerState gr) where
    (OneEventHandlerPerState a) <> (OneEventHandlerPerState b) = OneEventHandlerPerState (a <> b)
instance (Graph gr) => Monoid (OneEventHandlerPerState gr) where
    mempty = OneEventHandlerPerState mempty
    mappend = (<>)

instance (Graph gr) => Passable (OneEventHandlerPerState gr) where
    type Representation (OneEventHandlerPerState gr) = gr EnterExitState Happening
    accumulate (i, n , _, o) a = mappend (mappend (build i) (build $ fmap (fmap $ const n) o)) a
        where build = OneEventHandlerPerState . fromList . map (fmap (pure . event) . swap)
    test (StateMachine sm_name, g) (OneEventHandlerPerState es) =
        case filter (not . null . snd) $ map (st . fromJust . lab g *** filter ((> 1) . length) . group . sort) $ toList es of
        [] -> []
        ss -> [faultFor s es | (s, ess) <- ss, es <- ess]
        where htext = ", more than one handler for "
              faultFor (State s) es@(Event e:_)  = Fault ERROR (at s)       $ (disqualifyTag sm_name) ++ ": in state " ++ show (disqualifyTag s) ++ htext ++ "event " ++ show (disqualifyTag e) ++ " is forbidden"
              faultFor (State s)    (EventAny:_) = Fault ERROR (at s)       $ (disqualifyTag sm_name) ++ ": in state " ++ show (disqualifyTag s) ++ htext ++ "any-event is forbidden"
              faultFor  StateAny es@(Event e:_)  = Fault ERROR (at sm_name) $ (disqualifyTag sm_name) ++ ": in any-state"                        ++ htext ++ "event " ++ show (disqualifyTag e) ++ " is forbidden"
              faultFor  StateAny    (EventAny:_) = Fault ERROR (at sm_name) $ (disqualifyTag sm_name) ++ ": in any-state"                        ++ htext ++ "any-event is forbidden"
              faultFor         s           (e:_) = Fault BUG   (at sm_name) $ (disqualifyTag sm_name) ++ ": Invalid construction for " ++ show s ++ ", " ++ show e ++ ".  This is a bug in smudge."
