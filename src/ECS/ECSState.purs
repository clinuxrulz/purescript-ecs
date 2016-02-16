module ECS.ECSState where

import ECS.Scene

import Data.List (List)
import Data.List as List

newtype ECSState = ECSState {
  scene :: Scene,
  nextId :: Int,
  freeIds :: List Int
}
