{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Semantics.UniqueStateNames (
    UniqueStateNames
) where

import Grammars.Smudge (State(..), Annotated(..), StateMachineDeclarator(..))
import Model (EnterExitState(..), TaggedName, disqualifyTag)
import Semantics.Semantic (Passable(..), Severity(..), Fault(..))

import Data.Graph.Inductive.Graph (Graph)
import Data.Foldable (toList)
import Data.List (sort, intercalate, nub, (\\))
import Data.Monoid (Monoid(..))
import Data.Set (Set, singleton)

data UniqueStateNames = UniqueStateNames [State TaggedName] (Set (State TaggedName))
instance Monoid UniqueStateNames where
    mempty = UniqueStateNames mempty mempty
    mappend (UniqueStateNames sl ss) (UniqueStateNames sl' ss') =
        UniqueStateNames (mappend sl sl') (mappend ss ss')

instance (Graph gr) => Passable (gr EnterExitState a) UniqueStateNames where
    accumulate _ (_, _, ees, _) = mappend (UniqueStateNames [st ees] (singleton $ st ees))
    test (Annotated pos (StateMachineDeclarator sm_name), _) (UniqueStateNames sl ss) =
        case nub (sort sl \\ sort (toList ss)) \\ [StateEntry] of
        [] -> []
        rs -> [Fault ERROR pos $ (disqualifyTag sm_name) ++ ": State names cannot repeat: " ++
               (intercalate ", " $ [disqualifyTag name | State name <- rs] ++ ["_" | StateAny <- rs])]
