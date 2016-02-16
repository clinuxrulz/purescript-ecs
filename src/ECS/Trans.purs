module ECS.Trans
  ( ECST()
  , runECST
  , mkWithECST
  ) where

import ECS.ECSState (ECSState)
import ECS.SceneChanges (SceneChanges)

import Prelude (Unit, class Monad, bind, return, (<$>))
import Control.Monad.Writer.Trans (WriterT, runWriterT)
import Control.Monad.State.Trans (StateT, runStateT)
import Data.Tuple (Tuple(Tuple))

newtype ECST m a = ECST (StateT ECSState (WriterT SceneChanges m) a)

unECST :: forall m a. ECST m a -> StateT ECSState (WriterT SceneChanges m) a
unECST (ECST a) = a

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
