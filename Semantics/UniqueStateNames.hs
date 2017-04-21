{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

module Semantics.UniqueStateNames (
    UniqueStateNames
) where

import Grammars.Smudge (StateMachine(..), WholeState, State(..))
import Parsers.Id (at, showLineCol)
import Model (TaggedName, disqualifyTag)
import Semantics.Semantic (Passable(..), Severity(..), Fault(..))

import Data.List (sort, intercalate, group, (\\))
import Data.Monoid (Monoid(..))

data UniqueStateNames = UniqueStateNames [State TaggedName]
instance Monoid UniqueStateNames where
    mempty = UniqueStateNames mempty
    mappend (UniqueStateNames sl) (UniqueStateNames sl') =
        UniqueStateNames (mappend sl sl')

instance Passable UniqueStateNames where
    type Representation UniqueStateNames = [WholeState TaggedName]
    accumulate (s, _, _, _, _) = mappend (UniqueStateNames [s])
    test (StateMachine sm_name, _) (UniqueStateNames sl) =
        case group (sort sl \\ [StateEntry])  of
        [] -> []
        rs -> [Fault ERROR pos $ (disqualifyTag sm_name) ++ ": State names cannot repeat: " ++ state_listing |
               (pos, state_listing) <- [(at name, disqualifyTag name ++ " is given " ++ (show $ length ss) ++ " more time" ++ (if null $ tail ss then "" else "s") ++ " at " ++ intercalate ", " (map (\ (State s) -> showLineCol $ at s) ss)) | State name:ss@(_:_) <- rs] ++ [((at sm_name), "_") | StateAny:_:_ <- rs]]
