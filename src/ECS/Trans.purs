module ECS.Trans
  ( ECST()
  , runECST
  , mkWithECST
  ) where

import ECS.Class (class MonadECS)
import ECS.Component (Component)
import ECS.ECSState (ECSState)
import ECS.ECSState as ECSState
import ECS.SceneChanges (SceneChanges)

import Prelude ( ($), (<<<), Unit, class Functor, class Apply
               , class Applicative, class Bind, class Monad, map, apply, pure
               , bind, return, (<$>)
               )
import Control.Monad.Writer.Trans (WriterT, runWriterT)
import Control.Monad.State.Trans (StateT, runStateT)
import Data.Tuple (Tuple(Tuple))
import Data.Foldable (class Foldable)

newtype ECST m a = ECST (StateT ECSState (WriterT SceneChanges m) a)

unECST :: forall m a. ECST m a -> StateT ECSState (WriterT SceneChanges m) a
unECST (ECST a) = a

instance functorECST :: (Monad m) => Functor (ECST m) where
  map f (ECST m) = ECST $ map f m

instance applyECST :: (Monad m) => Apply (ECST m) where
  apply (ECST mf) (ECST ma) = ECST $ apply mf ma

instance applicativeECST :: (Monad m) => Applicative (ECST m) where
  pure a = ECST $ pure a

instance bindECST :: (Monad m) => Bind (ECST m) where
  bind (ECST m) f = ECST $ bind m (unECST <<< f)

instance monadECST :: (Monad m) => Monad (ECST m)

runECST :: forall m a. (Monad m) => ECSState -> ECST m a -> m { result :: a, state :: ECSState, changes :: SceneChanges }
runECST state (ECST m) = (\(Tuple (Tuple a s) w) -> { result: a, state: s, changes: w }) <$> runWriterT (runStateT m state)

mkWithECST :: forall m. (Monad m) => {
                getState :: m ECSState,
                putState :: ECSState -> m Unit,
                recordChanges :: SceneChanges -> m Unit
              }
           -> (forall a. ECST m a -> m a)
mkWithECST { getState, putState, recordChanges } =
  (\m -> do
    s <- getState
    { result, state, changes } <- runECST s m
    putState state
    recordChanges changes
    return result
  )

-- TODO: Finish this
--instance monadECSECST :: (Monad m) => MonadECS (ECST m) where
