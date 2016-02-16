module ECS.Scene
  ( Scene()
  , initScene
  , lookupEntity
  , addEntity
  , removeEntity
  ) where

import ECS.Entity (Entity, ident)

import Prelude (($))
import Data.Maybe (Maybe)
import Data.IntMap (IntMap)
import Data.IntMap as IntMap

newtype Scene = Scene {
  entities :: IntMap Entity
}

initScene :: Scene
initScene = Scene {
  entities: IntMap.empty
}

lookupEntity :: Int -> Scene -> Maybe Entity
lookupEntity entityId (Scene { entities }) = IntMap.lookup entityId entities

addEntity :: Entity -> Scene -> Scene
addEntity entity (Scene s@{ entities }) = Scene $ s { entities = IntMap.insert (ident entity) entity entities }

removeEntity :: Int -> Scene -> Scene
removeEntity entityId (Scene s@{ entities }) = Scene $ s { entities = IntMap.delete entityId entities }
