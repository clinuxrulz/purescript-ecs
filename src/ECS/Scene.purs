module ECS.Scene
  ( Scene()
  , lookupEntity
  ) where

import ECS.Entity (Entity)

import Data.Maybe (Maybe)
import Data.IntMap (IntMap)
import Data.IntMap as IntMap

newtype Scene = Scene {
  entities :: IntMap Entity
}

lookupEntity :: Int -> Scene -> Maybe Entity
lookupEntity entityId (Scene { entities }) = IntMap.lookup entityId entities
