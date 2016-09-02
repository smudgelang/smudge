module Semantics.UniqueStateNames (
    UniqueStateNames
) where

import Grammars.Smudge (State(..), Annotated(..), StateMachineDeclarator(..))
import Model (EnterExitState(..))
import Semantics.Semantic (Passable(..), Severity(..), Fault(..))

import Data.Foldable (toList)
import Data.List (sort, intercalate, (\\))
import Data.Monoid (Monoid(..))
import Data.Set (Set, singleton)

data UniqueStateNames = UniqueStateNames [State] (Set State)
instance Monoid UniqueStateNames where
    mempty = UniqueStateNames mempty mempty
    mappend (UniqueStateNames sl ss) (UniqueStateNames sl' ss') =
        UniqueStateNames (mappend sl sl') (mappend ss ss')

instance Passable UniqueStateNames where
    accumulate (_, _, ees, _) = mappend (UniqueStateNames [st ees] (singleton $ st ees))
    test (Annotated pos (StateMachineDeclarator sm_name), _) (UniqueStateNames sl ss) =
        case (sort sl \\ sort (toList ss)) of
        [] -> []
        rs -> [Fault ERROR pos $ sm_name ++ ": State names cannot repeat: " ++
               (intercalate ", " $ [name | State name <- rs])]
