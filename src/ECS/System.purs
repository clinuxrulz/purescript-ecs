module ECS.System where

import ECS.Component (Component)
import ECS.Entity (Entity)
import ECS.Scene (Scene)

import Prelude (Unit, ($), class Monad, bind, return)
import Control.Monad.State.Trans (StateT, execStateT, mapStateT)
import Control.Monad.State.Class (get, put)
import Control.Monad.Trans (lift)
import Data.Traversable (class Traversable, for)
import Data.Exists (Exists, runExists, mkExists)
import Data.Generic (GenericSpine)

newtype System m = System (Exists (SystemF m))

newtype SystemF m s = SystemF {
  name :: String,
  state :: s,
  handleSceneChanged :: Scene -> StateT s m Unit,
  handleEntityCreated :: Entity -> StateT s m Unit,
  handleEntityDestroyed :: Entity -> StateT s m Unit,
  handleComponentAdded :: Entity -> Component -> StateT s m Unit,
  handleComponentRemoved :: Entity -> Component -> StateT s m Unit,
  handleComponentValueChanged :: Entity -> String -> GenericSpine -> StateT s m Unit
}

mapSystem :: forall m1 m2. (forall a. m1 a -> m2 a) -> System m1 -> System m2
mapSystem f (System system) =
  System $
    runExists
      (\(SystemF {
        name: sysName,
        state: sysState,
        handleSceneChanged: sysHandleSceneChanged,
        handleEntityCreated: sysHandleEntityCreated,
        handleEntityDestroyed: sysHandleEntityDestroyed,
        handleComponentAdded: sysHandleComponentAdded,
        handleComponentRemoved: sysHandleComponentRemoved,
        handleComponentValueChanged: sysHandleComponentValueChanged
      }) ->
        mkExists $ SystemF {
          name: sysName,
          state: sysState,
          handleSceneChanged: (\scene -> mapStateT f (sysHandleSceneChanged scene)),
          handleEntityCreated: (\entity -> mapStateT f (sysHandleEntityCreated entity)),
          handleEntityDestroyed: (\entity -> mapStateT f (sysHandleEntityDestroyed entity)),
          handleComponentAdded: (\entity component -> mapStateT f (sysHandleComponentAdded entity component)),
          handleComponentRemoved: (\entity component -> mapStateT f (sysHandleComponentRemoved entity component)),
          handleComponentValueChanged: (\entity cName value -> mapStateT f (sysHandleComponentValueChanged entity cName value))
        }
      )
      system

getName :: forall m. System m -> String
getName (System system) = runExists (\(SystemF { name }) -> name) system

handleSceneChanged :: forall f m. (Traversable f, Monad m) => Scene -> StateT (f (System m)) m Unit
handleSceneChanged scene = do
  systems :: f (System m) <- get
  systems' <-
    lift $
      for systems (\(System system) ->
        runExists
          (\(SystemF a@{ state: s, handleSceneChanged: handler }) -> do
            s' <- execStateT (handler scene) s
            return $ System $ mkExists $ SystemF $ a { state = s' }
          )
          system
      )
  put systems'

handleEntityCreated :: forall f m. (Traversable f, Monad m) => Entity -> StateT (f (System m)) m Unit
handleEntityCreated entity = do
  systems :: f (System m) <- get
  systems' <-
    lift $
      for systems (\(System system) ->
        runExists
          (\(SystemF a@{ state: s, handleEntityCreated: handler }) -> do
            s' <- execStateT (handler entity) s
            return $ System $ mkExists $ SystemF $ a { state = s' }
          )
          system
      )
  put systems'

handleEntityDestroyed :: forall f m. (Traversable f, Monad m) => Entity -> StateT (f (System m)) m Unit
handleEntityDestroyed entity = do
  systems :: f (System m) <- get
  systems' <-
    lift $
      for systems (\(System system) ->
        runExists
          (\(SystemF a@{ state: s, handleEntityDestroyed: handler }) -> do
            s' <- execStateT (handler entity) s
            return $ System $ mkExists $ SystemF $ a { state = s' }
          )
          system
      )
  put systems'

handleComponentAdded :: forall f m. (Traversable f, Monad m) => Entity -> Component -> StateT (f (System m)) m Unit
handleComponentAdded entity component = do
  systems :: f (System m) <- get
  systems' <-
    lift $
      for systems (\(System system) ->
        runExists
          (\(SystemF a@{ state: s, handleComponentAdded: handler }) -> do
            s' <- execStateT (handler entity component) s
            return $ System $ mkExists $ SystemF $ a { state = s' }
          )
          system
      )
  put systems'

handleComponentRemoved :: forall f m. (Traversable f, Monad m) => Entity -> Component -> StateT (f (System m)) m Unit
handleComponentRemoved entity component = do
  systems :: f (System m) <- get
  systems' <-
    lift $
      for systems (\(System system) ->
        runExists
          (\(SystemF a@{ state: s, handleComponentRemoved: handler }) -> do
            s' <- execStateT (handler entity component) s
            return $ System $ mkExists $ SystemF $ a { state = s' }
          )
          system
      )
  put systems'

handleComponentValueChanged :: forall f m. (Traversable f, Monad m) => Entity -> String -> GenericSpine -> StateT (f (System m)) m Unit
handleComponentValueChanged entity cName value = do
  systems :: f (System m) <- get
  systems' <-
    lift $
      for systems (\(System system) ->
        runExists
          (\(SystemF a@{ state: s, handleComponentValueChanged: handler }) -> do
            s' <- execStateT (handler entity cName value) s
            return $ System $ mkExists $ SystemF $ a { state = s' }
          )
          system
      )
  put systems'
