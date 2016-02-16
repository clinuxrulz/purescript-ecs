module ECS.ComponentConstructor.Class where

import Data.Generic (class Generic)
import Type.Proxy (Proxy)

class (Generic a) <= ComponentConstructor a where
  componentName :: Proxy a -> String
