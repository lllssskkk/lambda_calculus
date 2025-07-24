module Language.STLC.Common where

import Control.Monad.Except (MonadError (throwError))

guardM :: (MonadError e m) => Bool -> e -> m ()
guardM False err = throwError err
guardM True _ = pure ()