module Semantics.OneInitialState (
    OneInitialState
) where

import Grammars.Smudge (State(..), Annotated(..), StateMachineDeclarator(..))
import Model (EnterExitState(..), Happening, disqualify)
import Semantics.Semantic (Passable(..), Severity(..), Fault(..))

import Data.Graph.Inductive.Graph (Adj, lab)
import Data.Monoid (Monoid(..))
import Data.List (intercalate)

data OneInitialState = OneInitialState (Adj Happening) (Adj Happening)
instance Monoid OneInitialState where
    mempty = OneInitialState mempty mempty
    mappend (OneInitialState is os) (OneInitialState is' os') =
        OneInitialState (mappend is is') (mappend os os')

instance Passable OneInitialState where
    accumulate (i, _, EnterExitState {st = StateEntry}, o) a = mappend (OneInitialState i o) a
    accumulate                                           _ a = a
    test (Annotated pos (StateMachineDeclarator sm_name), g) (OneInitialState is os) =
        case (length is, length os) of
        (0, 1) -> []
        (0, 0) -> [Fault ERROR pos $ (disqualify sm_name) ++ ": 1 initial state is required"]
        (0, n) -> [Fault ERROR pos $ (disqualify sm_name) ++ ": 1 initial state is required, but " ++ (show n) ++
                   " were given: " ++ (intercalate ", " $ [disqualify name | State name <- [st ees | Just ees <- map (lab g . snd) os]])]
        (_, _) -> [Fault BUG pos $ (disqualify sm_name) ++ ": Invalid entry state construction.  This is a bug in smudge."]
