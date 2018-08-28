-- Copyright 2018 Bose Corporation.
-- This software is released under the 3-Clause BSD License.
-- The license can be viewed at https://github.com/smudgelang/smudge/blob/master/LICENSE

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

module Language.Smudge.Passes.UniqueStateNames (
    UniqueStateNames
) where

import Language.Smudge.Grammar (StateMachine(..), WholeState, State(..))
import Language.Smudge.Parsers.Id (at, showLineCol)
import Language.Smudge.Semantics.Model (TaggedName, disqualifyTag)
import Language.Smudge.Passes.Passes (Passable(..), Severity(..), Fault(..))

import Data.List (sort, intercalate, group, (\\))
import Data.Monoid (Monoid(..))
import Data.Semigroup (Semigroup(..))

data UniqueStateNames = UniqueStateNames [State TaggedName]

instance Semigroup UniqueStateNames where
    (UniqueStateNames sl) <> (UniqueStateNames sl') =
        UniqueStateNames (sl <> sl')


instance Monoid UniqueStateNames where
    mempty = UniqueStateNames mempty
    mappend = (<>)

instance Passable UniqueStateNames where
    type Representation UniqueStateNames = [WholeState TaggedName]
    accumulate (s, _, _, _, _) = mappend (UniqueStateNames [s])
    test (StateMachine sm_name, _) (UniqueStateNames sl) =
        case group (sort sl \\ [StateEntry])  of
        [] -> []
        rs -> [Fault ERROR pos $ (disqualifyTag sm_name) ++ ": State names cannot repeat: " ++ state_listing |
               (pos, state_listing) <- [(at name, disqualifyTag name ++ " is given " ++ (show $ length ss) ++ " more time" ++ (if null $ tail ss then "" else "s") ++ " at " ++ intercalate ", " (map (\ (State s) -> showLineCol $ at s) ss)) | State name:ss@(_:_) <- rs] ++ [((at sm_name), "_") | StateAny:_:_ <- rs]]
