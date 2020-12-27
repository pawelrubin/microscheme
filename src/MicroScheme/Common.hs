module MicroScheme.Common where

import Control.Monad.State (MonadState (get, put))

-- | Run an action without changing the previous state.
withFrozenState :: MonadState s m => m a -> m a
withFrozenState action = do
  currentState <- get
  result <- action
  put currentState
  return result
