module ECS.Entity
  ( Entity()
  , mkEntity
  , ident
  , components
  , lookupComponent
  , lookupComponentValue
  , addComponent
  ) where

import ECS.ComponentConstructor.Class (class ComponentConstructor, componentName)
import ECS.Component (Component, name, value, mkComponent)

import Prelude (($), (>>=))
import Data.Maybe (Maybe)
import Data.Map (Map)
import Data.Map as Map
import Data.List (List)
import Data.Foldable (class Foldable, foldl)
import Type.Proxy (Proxy)

newtype Entity = Entity {
  ident :: Int,
  components :: Map String Component
}

mkEntity :: forall f. (Foldable f) => Int -> f Component -> Entity
mkEntity entityId cmps =
  Entity {
    ident: entityId,
    components: foldl (\cs c -> Map.insert (name c) c cs) Map.empty cmps
  }

ident :: Entity -> Int
ident (Entity { ident: x }) = x

components :: Entity -> List Component
components (Entity { components: x }) = Map.values x

lookupComponent :: forall a. (ComponentConstructor a) => Proxy a -> Entity -> Maybe Component
lookupComponent t (Entity { components: x }) = Map.lookup (componentName t) x

lookupComponentValue :: forall a. (ComponentConstructor a) => Proxy a -> Entity -> Maybe a
lookupComponentValue t entity = lookupComponent t entity >>= value t

addComponent :: Component -> Entity -> Entity
addComponent cmp (Entity s@{ components: cmps }) = Entity $ s { components = Map.insert (name cmp) cmp cmps }

addComponentValue :: forall a. (ComponentConstructor a) => a -> Entity -> Entity
addComponentValue a entity = addComponent (mkComponent a) entity

removeComponent :: forall a. (ComponentConstructor a) => Proxy a -> Entity -> Entity
removeComponent t (Entity s@{ components: x }) = Entity $ s { components = Map.delete (componentName t) x }
