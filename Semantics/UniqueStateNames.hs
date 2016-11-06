{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

module Semantics.UniqueStateNames (
    UniqueStateNames
) where

import Grammars.Smudge (WholeState, State(..), Annotated(..), StateMachineDeclarator(..))
import Model (TaggedName, disqualifyTag)
import Semantics.Semantic (Passable(..), Severity(..), Fault(..))

import Data.Foldable (toList)
import Data.List (sort, intercalate, nub, (\\))
import Data.Monoid (Monoid(..))
import Data.Set (Set, singleton)

data UniqueStateNames = UniqueStateNames [State TaggedName] (Set (State TaggedName))
instance Monoid UniqueStateNames where
    mempty = UniqueStateNames mempty mempty
    mappend (UniqueStateNames sl ss) (UniqueStateNames sl' ss') =
        UniqueStateNames (mappend sl sl') (mappend ss ss')

instance Passable UniqueStateNames where
    type Representation UniqueStateNames = [WholeState TaggedName]
    accumulate (s, _, _, _, _) = mappend (UniqueStateNames [s] (singleton s))
    test (Annotated pos (StateMachineDeclarator sm_name), _) (UniqueStateNames sl ss) =
        case nub (sort sl \\ sort (toList ss)) \\ [StateEntry] of
        [] -> []
        rs -> [Fault ERROR pos $ (disqualifyTag sm_name) ++ ": State names cannot repeat: " ++
               (intercalate ", " $ [disqualifyTag name | State name <- rs] ++ ["_" | StateAny <- rs])]
