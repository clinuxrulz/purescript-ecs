module ECS.Entity
  ( Entity()
  , ident
  , components
  ) where

import ECS.ComponentConstructor.Class (class ComponentConstructor, componentName)
import ECS.Component (Component, value)

import Prelude ((>>=))
import Data.Maybe (Maybe)
import Data.Map (Map)
import Data.Map as Map
import Data.List (List)
import Type.Proxy (Proxy)

newtype Entity = Entity {
  ident :: Int,
  components :: Map String Component
}

ident :: Entity -> Int
ident (Entity { ident: x }) = x

components :: Entity -> List Component
components (Entity { components: x }) = Map.values x

lookupComponent :: forall a. (ComponentConstructor a) => Proxy a -> Entity -> Maybe Component
lookupComponent t (Entity { components: x }) = Map.lookup (componentName t) x

lookupComponentValue :: forall a. (ComponentConstructor a) => Proxy a -> Entity -> Maybe a
lookupComponentValue t entity = lookupComponent t entity >>= value t
