module ECS.ECSState where

import ECS.Component (Component)
import ECS.Entity (mkEntity)
import ECS.Scene (Scene)
import ECS.Scene (addEntity, removeEntity) as Scene

import Prelude ((+), ($))
import Data.Tuple (Tuple(Tuple), fst, snd)
import Data.Maybe (maybe')
import Data.List (List)
import Data.List as List
import Data.Foldable (class Foldable)

newtype ECSState = ECSState {
  scene :: Scene,
  nextId :: Int,
  freeIds :: List Int
}

allocId :: ECSState -> Tuple Int ECSState
allocId (ECSState s@{ nextId, freeIds }) =
  maybe'
    (\_ -> Tuple nextId (ECSState $ s { nextId = nextId + 1 }))
    (\{ head, tail } -> Tuple head (ECSState $ s { freeIds = tail }))
    (List.uncons freeIds)

freeId :: Int -> ECSState -> ECSState
freeId ident (ECSState s@{ freeIds }) =
  ECSState $ s { freeIds = List.Cons ident freeIds }

createEntity :: forall f. (Foldable f) => f Component -> ECSState -> Tuple Int ECSState
createEntity cmps st =
  let tmp = allocId st
      ident = fst tmp
      st' = snd tmp
  in
  case st' of
    ECSState s@{ scene } ->
      Tuple ident (ECSState $ s { scene = Scene.addEntity (mkEntity ident cmps) scene })

destroyEntity :: Int -> ECSState -> ECSState
destroyEntity entityId st =
  let st' = freeId entityId st
  in
  case st' of
    ECSState s@{ scene } ->
      ECSState $ s { scene = Scene.removeEntity entityId scene }
