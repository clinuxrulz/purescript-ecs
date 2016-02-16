module ECS.SceneChanges where

import Prelude (class Semigroup)
import Data.Monoid (class Monoid)

newtype SceneChanges = SceneChanges {}

instance semigroupSceneChanges :: Semigroup SceneChanges where
  append a _ = a

instance monoidSceneChanges :: Monoid SceneChanges where
  mempty = SceneChanges {}
