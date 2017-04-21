{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

module Semantics.OneInitialState (
    OneInitialState
) where

import Grammars.Smudge (StateMachine(..), State(..))
import Parsers.Id (at, showLineCol)
import Model (EnterExitState(..), Happening, disqualifyTag)
import Semantics.Semantic (Passable(..), Severity(..), Fault(..))

import Data.Graph.Inductive.Graph (Graph, Adj, lab)
import Data.Monoid (Monoid(..))
import Data.List (intercalate)

data (Graph gr) => OneInitialState gr = OneInitialState (Adj Happening) (Adj Happening)
instance (Graph gr) => Monoid (OneInitialState gr) where
    mempty = OneInitialState mempty mempty
    mappend (OneInitialState is os) (OneInitialState is' os') =
        OneInitialState (mappend is is') (mappend os os')

instance (Graph gr) => Passable (OneInitialState gr) where
    type Representation (OneInitialState gr) = gr EnterExitState Happening
    accumulate (i, _, EnterExitState {st = StateEntry}, o) a = mappend (OneInitialState i o) a
    accumulate                                           _ a = a
    test (StateMachine sm_name, g) (OneInitialState is os) =
        case (length is, length os) of
        (0, 1) -> []
        (0, 0) -> [Fault ERROR (at sm_name) $ (disqualifyTag sm_name) ++ ": 1 initial state is required"]
        (0, n) -> [Fault ERROR (at sm_name) $ (disqualifyTag sm_name) ++ ": 1 initial state is required, but " ++ (show n) ++
                   " were given: " ++ (intercalate ", " $ [disqualifyTag name ++ " " ++ showLineCol (at name) | State name <- [st ees | Just ees <- map (lab g . snd) os]])]
        (_, _) -> [Fault BUG (at sm_name) $ (disqualifyTag sm_name) ++ ": Invalid entry state construction.  This is a bug in smudge."]
