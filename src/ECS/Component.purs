module ECS.Component
  ( Component()
  , name
  , value
  , mkComponent
  ) where

import ECS.ComponentConstructor.Class (class ComponentConstructor, componentName)

import Data.Maybe (Maybe)
import Data.Generic (class Generic, GenericSpine, fromSpine, toSpine)
import Type.Proxy (Proxy(Proxy))

newtype Component = Component {
  name :: String,
  value :: GenericSpine
}

name :: Component -> String
name (Component { name: x }) = x

value :: forall a. (Generic a) => Proxy a -> Component -> Maybe a
value _ (Component { value: x }) = fromSpine x

mkComponent :: forall a. (ComponentConstructor a) => a -> Component
mkComponent a = Component { name: componentName (Proxy :: Proxy a), value: toSpine a }
