module Util where

import qualified Control.Monad.Except          as E
import qualified Control.Monad.State.Class     as S

throw :: E.MonadError e m => e -> m a
throw = E.throwError

locally :: S.MonadState s m => (s -> m b) -> m b
locally f = do
    m   <- S.get
    res <- f m
    S.put m

    pure res

(...) :: (b -> c) -> (a1 -> a2 -> b) -> a1 -> a2 -> c
(...) = (.) . (.)
