{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

module Semantics.OneInitialState (
    OneInitialState
) where

import Grammars.Smudge (State(..), Annotated(..), StateMachineDeclarator(..))
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
    test (Annotated pos (StateMachineDeclarator sm_name), g) (OneInitialState is os) =
        case (length is, length os) of
        (0, 1) -> []
        (0, 0) -> [Fault ERROR pos $ (disqualifyTag sm_name) ++ ": 1 initial state is required"]
        (0, n) -> [Fault ERROR pos $ (disqualifyTag sm_name) ++ ": 1 initial state is required, but " ++ (show n) ++
                   " were given: " ++ (intercalate ", " $ [disqualifyTag name | State name <- [st ees | Just ees <- map (lab g . snd) os]])]
        (_, _) -> [Fault BUG pos $ (disqualifyTag sm_name) ++ ": Invalid entry state construction.  This is a bug in smudge."]
