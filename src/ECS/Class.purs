module ECS.Class where

import ECS.ComponentConstructor.Class (class ComponentConstructor)
import ECS.Component (Component)
import ECS.Scene (Scene)

import Prelude (Unit, class Monad)
import Data.Foldable (class Foldable)
import Type.Proxy (Proxy)

class (Monad m) <= MonadECS m where
  getScene :: m Scene
  createEntity :: forall f. (Foldable f) => f Component -> m Int
  destroyEntity :: Int -> m Unit
  addComponent :: forall a. (ComponentConstructor a) => Int -> a -> m Unit
  removeComponent :: forall a. (ComponentConstructor a) => Int -> Proxy a -> m Unit
  changeComponent :: forall a. (ComponentConstructor a) => Int -> a -> m Unit
